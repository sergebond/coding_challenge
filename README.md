coding_challenge
=====

An OTP application

RUN
-----

    $ rebar3 shell

TEST
-----
___
**SORT AND GET A BASH SCRIPT**

_Request_

    curl --location 'localhost:8080/api/1/json/fold' \
    --header 'Content-Type: application/json' \
    --data '{
        "tasks": [
            {
                "name": "task-1",
                "command": "touch /tmp/file1"
            },
            {
                "name": "task-2",
                "command": "cat /tmp/file1",
                "requires": [
                    "task-3"
                    ]
            },
            {
                "name": "task-3",
                "command": "echo '\''Hello World!'\'' > /tmp/file1",
                "requires": [
                    "task-1"
                    ]
            },
            {
            "name": "task-4",
            "command": "rm /tmp/file1",
            "requires": [
                    "task-2",
                    "task-3"
                ]
            }
        ]
    }'

Response

    #!/usr/bin/env bash
    touch /tmp/file1
    echo 'Hello World!' > /tmp/file1
    cat /tmp/file1
    rm /tmp/file1

___
**ONLY SORT**

_Request_

    curl --location 'localhost:8080/api/1/json/sort' \
    --header 'Content-Type: application/json' \
    --data '{
        "tasks": [
            {
                "name": "task-1",
                "command": "touch /tmp/file1"
            },
            {
                "name": "task-2",
                "command": "cat /tmp/file1",
                "requires": [
                    "task-3"
                    ]
            },
            {
                "name": "task-3",
                "command": "echo '\''Hello World!'\'' > /tmp/file1",
                "requires": [
                    "task-1"
                    ]
            },
            {
            "name": "task-4",
            "command": "rm /tmp/file1",
            "requires": [
                    "task-2",
                    "task-3"
                ]
            }
        ]
    }'

Response

    {
        "status": "ok",
        "tasks": [
            {
                "command": "touch /tmp/file1",
                "name": "task-1"
            },
            {
                "command": "echo 'Hello World!' > /tmp/file1",
                "name": "task-3"
            },
            {
                "command": "cat /tmp/file1",
                "name": "task-2"
            },
            {
                "command": "rm /tmp/file1",
                "name": "task-4"
            }
        ]
    }