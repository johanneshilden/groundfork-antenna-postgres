# groundfork-antenna-postgres

#### GET /log

```bash
$ curl --user root:root http://localhost:3333/log 
```

```bash
$ curl -G \
    --user root:root \
    --data-urlencode page=1 \
    --data-urlencode size=15 \
    http://localhost:3333/log
```

#### POST /log/reset

```bash
$ curl --user root:root -X POST http://localhost:3333/log/reset  
```

#### GET /nodes

```bash
$ curl --user root:root http://localhost:3333/nodes
```

#### POST /nodes

| Property      | Type          | Extra                      |   
| ------------- |---------------|----------------------------| 
| name          | string        |                            |
| type          | string        | 'device' &#124; 'virtual'  |
| password      | string        | Applies to device nodes only. |
| locked        | boolean       |                            |

```bash
$ curl --user root:root \
    -X POST \
    -H "Content-Type: application/json" \
    -d '{"name":"virtual-1","type":"virtual","locked":false}' \
    http://localhost:3333/nodes  
```

```bash
$ curl --user root:root \
    -X POST \
    -H "Content-Type: application/json" \
    -d '{"name":"device-1","password":"pass","type":"device","locked":false}' \
    http://localhost:3333/nodes 
```

#### PUT /nodes/:id

| Property      | Type          | Extra                      |   
| ------------- |---------------|----------------------------| 
| name          | string        |                            |
| password      | string        | Applies to device nodes only. |
| locked        | boolean       |                            |

```
$ curl --user root:root \ 
     -X PUT \
     -H "Content-Type: application/json" \
     -d '{"name":"new-name"}' \
     http://localhost:3333/nodes/3215
```

#### DELETE /nodes/:id

```bash
$ curl --user root:root -X DELETE http://localhost:3333/nodes/3216
```

#### GET /ping

```bash
$ curl http://localhost:3333/ping
```

```
Pong!
```

#### GET /sp

```bash
$ curl --user root:root http://localhost:3333/sp 
```

```json
{
   "status"  : "success",
   "message" : "OK",
   "body"    : {
      "syncPoint" : 1438174179
   }
}
```

#### POST /sync

| Property      | Type          | Extra                      |   
| ------------- |---------------|----------------------------| 
| targets       | array         |                            |
| syncPoint     | string | number | A timestamp, or '*'.     |
| commit        | array         | Action log.                |

```bash 
$ cat request.json
```

```json
{
    "targets": ["root"],
    "syncPoint": 0,
    "commit": [ 
        { 
            "up": { 
                "method": "POST", 
                "resource": "posts", 
                "payload": {
                    "title": "My first post",
                    "body": "In omnium maluisset eum, per putent singulis tincidunt id.",
                    "user": "bob"
                }
            }, 
            "down": { 
                "method": "DELETE", 
                "resource": "posts/1" 
            },                   
            "index": 1,                                                         
            "timestamp": 1438388891 
        } 
    ] 
}
```

```bash
$ curl --user root:root \
     -X POST \ 
     -H "Content-Type: application/json" \
     -d @request.json \
     http://localhost:3333/sync 
```

```json
{
    "status": "success",
    "forward": [
        {
            "payload": {
                "body": "In omnium maluisset eum, per putent singulis tincidunt id.",
                "user": "bob",
                "title": "My first post"
            },
            "method": "POST",
            "resource": "posts"
        }
    ],
    "reverse": [],
    "syncPoint": "*"
}
```
