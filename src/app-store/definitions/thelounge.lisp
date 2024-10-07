(app
    ; name of the app is taken from file name
    (version "4.4.3")
    (ports 9000)
    (containers
        (container
            (name "thelounge")
            (image "ghcr.io/thelounge/thelounge:4.4.3")
            (volumes 
                ("data" "/var/opts/thelounge")))))
