(define-app
    ; name of the app is taken from file name
    (version "1.10.0")
    (ports 7860)
    (containers
        (container
            (name "automatic1111")
            (build "localhost/johnaic/automatic1111:1.10.0")
            (environment
                ("venv_dir" "/apps/deps/venv")
                ("install_dir" "/apps/automatic1111")
                ("MY_GRADIO_USERNAME" "sd-web-user")
                ("MY_GRADIO_PASSWORD" ,(gen-password))
                ("MY_API_USERNAME" "sd-api-user")
                ("MY_API_PASSWORD" ,(gen-password)))
            (volumes
                ("deps" "/apps/deps/")
                ("data" "/apps/automatic1111/")
                ("init.sh" "/apps/init.sh"))
            (command "bash /apps/init.sh")
            (additional-flags "--device nvidia.com/gpu=all"))))
