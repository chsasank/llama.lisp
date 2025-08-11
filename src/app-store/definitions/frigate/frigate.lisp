(define-app
  (version "3.9")
  (ports 8971 8554 8555/tcp 8555/udp)
  (containers
    (container
      (name "frigate")
      (image "ghcr.io/blakeblackshear/frigate:stable")
      (restart "unless-stopped")
      (devices
        ("/dev/bus/usb" "/dev/bus/usb"))
      (volumes
        ("/etc/localtime" "/etc/localtime:ro")
        ("./config" "/config")
        ("./storage" "/media/frigate"))
      (environment
        (FRIGATE_RTSP_PASSWORD "password")))))
