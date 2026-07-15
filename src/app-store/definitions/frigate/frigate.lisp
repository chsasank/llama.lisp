(define-app
  (version "3.9")
  (ports 8971)
  (containers
    (container
      (name "frigate")
      (image "ghcr.io/blakeblackshear/frigate:0.15.2-tensorrt")
      (volumes
        ("/etc/localtime" "/etc/localtime:ro")
        ("config" "/config")
        ("storage" "/media/frigate/storage"))
      (additional-flags "--device nvidia.com/gpu=all"))))
