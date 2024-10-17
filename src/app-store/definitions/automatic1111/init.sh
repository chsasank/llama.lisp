set -e
bash webui.sh -f --xformers --listen --api --api-auth $MY_API_USERNAME:$MY_API_PASSWORD --gradio-auth $MY_GRADIO_USERNAME:$MY_GRADIO_PASSWORD
