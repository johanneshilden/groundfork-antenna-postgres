# groundfork-antenna-postgres

#### GET /log

```bash
$ curl --user root:root http://localhost:3333/log 
```

```bash
$ curl -G \
    --user root:root http://localhost:3333/log \
    --data-urlencode page=1 \
    --data-urlencode size=15
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

#### DELETE /nodes/:id

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
