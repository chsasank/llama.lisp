import os
import json

import transformers
import torch
from layers import SinusoidalPositionalEmbedding, DecoderLayer


class ParlerTTS(torch.nn.Module):
    def __init__(self, checkpoint_dir):
        super().__init__()
        self.checkpoint_dir = checkpoint_dir
        self.config = json.load(
            open(os.path.join(checkpoint_dir, "indic_parler_tts_config.json"))
        )
        self.description_tokenizer = transformers.AutoTokenizer.from_pretrained(
            "google/flan-t5-large"
        )
        self.prompt_tokenizer = transformers.AutoTokenizer.from_pretrained(
            "ai4bharat/indic-parler-tts"
        )
        self.description_encoder = transformers.T5EncoderModel.from_pretrained(
            os.path.join(checkpoint_dir, "./parler_tts_text_encoder")
        )
        self.embed_prompt = torch.nn.Embedding(
            self.config["vocab_size"], self.config["decoder"]["hidden_size"]
        )
        self.embed_audio = torch.nn.ModuleList(
            [
                torch.nn.Embedding(
                    self.config["decoder"]["vocab_size"] + 1,
                    self.config["decoder"]["hidden_size"],
                )
                for _ in range(self.config["decoder"]["num_codebooks"])
            ]
        )
        self.embed_position = SinusoidalPositionalEmbedding(
            self.config["decoder"]["max_position_embeddings"],
            self.config["decoder"]["hidden_size"],
        )
        self.decoder_layers = torch.nn.ModuleList(
            [
                DecoderLayer(
                    embed_dim=self.config["decoder"]["hidden_size"],
                    num_heads=self.config["text_encoder"]["num_heads"],
                    ffn_dim=self.config["decoder"]["ffn_dim"],
                    dropout_p=self.config["decoder"]["dropout"],
                    activation_dropout_p=self.config["decoder"]["attention_dropout"],
                    activation=self.config["decoder"]["activation_function"],
                )
                for _ in range(self.config["decoder"]["num_hidden_layers"])
            ]
        )
        self.layer_norm = torch.nn.LayerNorm(self.config["decoder"]["hidden_size"])
        self.lm_heads = torch.nn.Linear(
            self.config["decoder"]["hidden_size"],
            (
                self.config["decoder"]["vocab_size"]
                * self.config["decoder"]["num_codebooks"]
            ),
            bias=False,
        )
        self._load_weights()

    def _load_weights(self):
        def _sub_state_dict(prefix, model_weights):
            prefix = prefix + "."
            return {
                k.replace(prefix, ""): v
                for k, v in model_weights.items()
                if prefix in k
            }

        model_weights = torch.load(
            os.path.join(self.checkpoint_dir, "parles_tts_state_dict.pt"),
            map_location=torch.device("cpu"),
            weights_only=True,
        )

        self.embed_prompt.load_state_dict(
            _sub_state_dict("embed_prompts", model_weights)
        )
        self.embed_audio.load_state_dict(
            _sub_state_dict("decoder.model.decoder.embed_tokens", model_weights)
        )
        self.embed_position.weights = model_weights[
            "decoder.model.decoder.embed_positions.weights"
        ]
        self.decoder_layers.load_state_dict(
            _sub_state_dict("decoder.model.decoder.layers", model_weights)
        )
        self.layer_norm.load_state_dict(
            _sub_state_dict("decoder.model.decoder.layer_norm", model_weights)
        )
        self.lm_heads.load_state_dict(
            _sub_state_dict("decoder.lm_heads", model_weights)
        )


model = ParlerTTS("/home/sasank/code/inference-opt/checkpoints")
print(model)
