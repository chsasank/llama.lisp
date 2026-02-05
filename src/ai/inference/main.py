import torch
import transformers


class QwenModelRunner:
    def __init__(self):
        self.model_name = "Qwen/Qwen2.5-0.5B-Instruct"
        self.model = transformers.AutoModelForCausalLM.from_pretrained(
            self.model_name, torch_dtype="auto", device_map="auto"
        ).eval()
        self.tokenizer = transformers.AutoTokenizer.from_pretrained(self.model_name)

    def model_prefill(self, messages_batch):
        """Returns model state for a prefill/encoding stage"""
        # tokenize text
        texts = [
            self.tokenizer.apply_chat_template(
                messages, tokenize=False, add_generation_prompt=True
            )
            for messages in messages_batch
        ]
        inputs = self.tokenizer(texts, padding=True, return_tensors="pt")
        input_ids = inputs.input_ids.to(self.model.device)
        attention_mask = inputs.attention_mask.to(self.model.device)

        # run model
        model_outputs = self.model(
            input_ids=input_ids,
            attention_mask=attention_mask,
            past_key_values=None,
            use_cache=True,
            return_dict=True,
        )

        # Convert past_key_values (kv cache) into tensor for easy merging etc
        # For now we're using transformers 4.46.3, so we're getting legacy
        # cache tuples etc.
        past_key_values = model_outputs.past_key_values
        past_key_values = torch.stack([torch.stack(x) for x in past_key_values])

        model_state = {
            "input_ids": input_ids,
            "attention_mask": attention_mask,
            "past_key_values": past_key_values,
            "logits": model_outputs.logits,
        }
        model_state = self._sample(model_state)
        return model_state

    def _sample(self, model_state):
        next_token_logits = model_state["logits"][:, -1, :]

        # do greedy decoding for now
        next_tokens = torch.argmax(next_token_logits, dim=-1, keepdim=True)

        # grow attenton_mask
        attention_mask = model_state["attention_mask"]
        batch_size = attention_mask.shape[0]
        attention_mask = torch.cat(
            [
                attention_mask,
                torch.ones(
                    (batch_size, 1),
                    dtype=attention_mask.dtype,
                    device=attention_mask.device,
                ),
            ],
            dim=1,
        )

        return {
            "input_ids": next_tokens,
            "attention_mask": attention_mask,
            "past_key_values": model_state["past_key_values"],
            "logits": model_state["logits"],
        }

    def stopping_criteria(self, model_state):
        """Returns 0/1 for each of the batch if it has to be stopped"""
        return model_state["input_ids"][:, -1] == self.tokenizer.eos_token_id

    def model_step(self, model_state):
        model_outputs = self.model(
            input_ids=model_state["input_ids"],
            attention_mask=model_state["attention_mask"],
            past_key_values=model_state["past_key_values"],
            use_cache=True,
            return_dict=True,
        )

        # Convert past_key_values (kv cache) into tensor for easy merging etc
        # For now we're using transformers 4.46.3, so we're getting legacy
        # cache tuples etc.
        past_key_values = model_outputs.past_key_values
        past_key_values = torch.stack([torch.stack(x) for x in past_key_values])

        model_state = {
            "input_ids": model_state["input_ids"],
            "attention_mask": model_state["attention_mask"],
            "past_key_values": past_key_values,
            "logits": model_outputs.logits,
        }
        model_state = self._sample(model_state)
        return model_state

    def print_state(self, model_state):
        for k, v in model_state.items():
            print(k, v.shape)


with torch.inference_mode():
    runner = QwenModelRunner()
    messages_batch = [
        # Prompt 1
        [
            {
                "role": "system",
                "content": "You are Qwen, created by Alibaba Cloud.",
            },
            {"role": "user", "content": "What is Grok in one short sentence?"},
        ],
        # Prompt 2
        [
            {
                "role": "system",
                "content": "You are Qwen, created by Alibaba Cloud.",
            },
            {"role": "user", "content": "Give me a haiku about the moon."},
        ],
        [
            {
                "role": "system",
                "content": "You are Qwen, created by Alibaba Cloud.",
            },
            {"role": "user", "content": "Give me a haiku about the moon."},
        ],
    ]
    active = torch.ones(
        len(messages_batch), dtype=torch.bool, device=runner.model.device
    )

    state = runner.model_prefill(messages_batch)
    print([runner.tokenizer.decode(x) for x in state["input_ids"]])
    active = active & ~runner.stopping_criteria(state)
    while active.any():
        state = runner.model_step(state)
        print([runner.tokenizer.decode(x) for x in state["input_ids"]])
        active = active & ~runner.stopping_criteria(state)

    runner.print_state(state)
