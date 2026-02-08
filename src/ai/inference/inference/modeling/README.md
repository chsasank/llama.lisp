Install deps

```
# cd to current dir
pip install -e ../../tts/parler-tts-hf/
python parler_tts_runner.py
```

```
...
-------------
input_ids <class 'NoneType'>
encoder_outputs <class 'transformers.modeling_outputs.BaseModelOutput'>
past_key_values <class 'transformers.cache_utils.EncoderDecoderCache'>
decoder_input_ids torch.Size([27, 1])
attention_mask torch.Size([3, 30])
decoder_attention_mask torch.Size([3, 397])
head_mask <class 'NoneType'>
decoder_head_mask <class 'NoneType'>
cross_attn_head_mask <class 'NoneType'>
prompt_hidden_states <class 'NoneType'>
prompt_attention_mask torch.Size([3, 18])
use_cache <class 'bool'>
cache_position torch.Size([1])
inputs_embeds torch.Size([3, 19, 1024])
-------------
input_ids <class 'NoneType'>
encoder_outputs <class 'transformers.modeling_outputs.BaseModelOutput'>
past_key_values <class 'transformers.cache_utils.EncoderDecoderCache'>
decoder_input_ids torch.Size([27, 1])
attention_mask torch.Size([3, 30])
decoder_attention_mask torch.Size([3, 398])
head_mask <class 'NoneType'>
decoder_head_mask <class 'NoneType'>
cross_attn_head_mask <class 'NoneType'>
prompt_hidden_states <class 'NoneType'>
prompt_attention_mask torch.Size([3, 18])
use_cache <class 'bool'>
cache_position torch.Size([1])
inputs_embeds torch.Size([3, 19, 1024])
-------------
input_ids <class 'NoneType'>
encoder_outputs <class 'transformers.modeling_outputs.BaseModelOutput'>
past_key_values <class 'transformers.cache_utils.EncoderDecoderCache'>
decoder_input_ids torch.Size([27, 1])
attention_mask torch.Size([3, 30])
decoder_attention_mask torch.Size([3, 399])
head_mask <class 'NoneType'>
decoder_head_mask <class 'NoneType'>
cross_attn_head_mask <class 'NoneType'>
prompt_hidden_states <class 'NoneType'>
prompt_attention_mask torch.Size([3, 18])
use_cache <class 'bool'>
cache_position torch.Size([1])
inputs_embeds torch.Size([3, 19, 1024])
-------------
step10:  5571.6493129730225 ms
End:  5805.376052856445 ms
```