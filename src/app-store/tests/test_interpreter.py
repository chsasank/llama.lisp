from app_store.interpreter import config_lisp
from app_store.parser import parse_sexp, lookup_sexp


def test_basic():
    assert config_lisp("hi") == "hi"
    assert config_lisp(10) == 10
    assert config_lisp(False) == False
    assert config_lisp(["hi", "hello"]) == ["hi", "hello"]
    assert config_lisp(["hi", ["hello", "world"]]) == ["hi", ["hello", "world"]]


def test_unquote():
    assert config_lisp(["unquote", "hello"], env={"hello": "world"}) == "world"
    assert config_lisp(["hi", ["unquote", "hello"]], env={"hello": "world"}) == [
        "hi",
        "world",
    ]
    assert config_lisp(
        ["hi", ["yo", ["unquote", "hello"], "yoyo"]], env={"hello": "world"}
    ) == ["hi", ["yo", "world", "yoyo"]]

    assert config_lisp(["unquote", ["fn", 10]], env={"fn": lambda x: x + 10}) == 20

    assert (
        config_lisp(["unquote", ["add", 10, 20]], env={"add": lambda x, y: x + y}) == 30
    )


def test_let():
    assert config_lisp(["let", [["hi", "world"]], ["hello", ["unquote", "hi"]]]) == [
        "hello",
        "world",
    ]
    # variable evaluation
    assert config_lisp(
        ["let", [["hi", ["unquote", "world"]]], ["hello", ["unquote", "hi"]]],
        env={"world": "sasank"},
    ) == [
        "hello",
        "sasank",
    ]
    # multiple varialbes
    assert config_lisp(
        [
            "let",
            [["hi", ["unquote", "world"]], ["yo", "yoyo"]],
            ["hello", ["unquote", "hi"], ["unquote", "yo"]],
        ],
        env={"world": "sasank"},
    ) == ["hello", "sasank", "yoyo"]


def test_app_definition():
    out = config_lisp(
        parse_sexp(
            """
    (define-app
        ; name of the app is taken from file name
        (version "1.21.4")
        (ports 3000 22)
        (let ((db-name "gitea")
            (db-user "gitea")
            (db-password ,(gen-password)))
            (containers
                (container
                    (name "gitea")
                    (image "docker.io/gitea/gitea:1.21.4")
                    (volumes 
                        ("gitea-data" "/data"))
                    (environment
                        ("GITEA__database__DB_TYPE" "postgres")
                        ("GITEA__database__HOST" "localhost:5432")
                        ("GITEA__database__NAME" ,db-name)
                        ("GITEA__database__USER" ,db-user)
                        ("GITEA__database__PASSWD" ,db-password)))
                (container
                    (name "db")
                    (image "docker.io/postgres:14")
                    (volumes
                        ("postgres-data" "/var/lib/postgresql/data"))
                    (environment
                        ("POSTGRES_USER" ,db-name)
                        ("POSTGRES_PASSWORD" ,db-user)
                        ("POSTGRES_DB" ,db-password))))))    
    """
        )
    )
    containers = lookup_sexp(out, "containers")
    env_1 = lookup_sexp(containers[0], "environment")
    env_2 = lookup_sexp(containers[1], "environment")
    assert env_2[0][1] == "gitea"
    assert env_1[-1][1] == env_2[-1][1]

    out = config_lisp(
        parse_sexp(
            """(define-app
    (version "3.3.2")
    (ports 3000)
    (url "https://www.discourse.org/")
    (let ((db-user "bn_discourse")
          (db-password ,(gen-password))
          (db-name "bitnami_discourse")
          (redis-password ,(gen-password))
          (discourse-image "docker.io/bitnami/discourse:3.3.2")
          (discourse-host "www.example.com"))
        (containers
            (container
                (name "sidekiq")
                (image ,discourse-image)
                (volumes
                    ("sidekiq_data" "/bitnami/discourse"))
                (command "/opt/bitnami/scripts/discourse-sidekiq/run.sh")
                (environment
    `               ("DISCOURSE_HOST" ,discourse-host)
                    ("DISCOURSE_DATABASE_HOST" "localhost")
                    ("DISCOURSE_DATABASE_PORT_NUMBER" 5432)
                    ("DISCOURSE_DATABASE_USER" ,db-user)
                    ("DISCOURSE_DATABASE_NAME" ,db-name)
                    ("DISCOURSE_DATABASE_PASSWORD" ,db-password)
                    ("DISCOURSE_REDIS_HOST" "localhost")
                    ("DISCOURSE_REDIS_PORT_NUMBER" 6379)
                    ("DISCOURSE_REDIS_PASSWORD" ,redis-password))))))"""
        )
    )

    print(out)


if __name__ == "__main__":
    test_basic()
    test_unquote()
    test_let()
    test_app_definition()
