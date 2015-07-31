# groundfork-antenna-postgres

#### GET /log

```
$ curl --user root:root http://localhost:3333/log 
```

```
$ curl -G --user root:root http://localhost:3333/log --data-urlencode page=1 --data-urlencode size=15
```

#### POST /log/reset

```
$ curl --user root:root -X POST http://localhost:3333/log/reset  
```

#### GET /nodes

```
$ curl --user root:root http://localhost:3333/nodes
```

#### POST /nodes



#### PUT /nodes/:id

#### DELETE /nodes/:id

#### GET /ping

```
$ curl http://localhost:3333/ping
Pong!
```

#### GET /sp

```
$ curl --user root:root http://localhost:3333/sp 
```

```json
{
   "status"  : "success",
   "message" :"OK",
   "body"    : {
      "syncPoint" : 1438174179
   }
}
```

#### POST /sync
