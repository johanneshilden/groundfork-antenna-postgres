# groundfork-antenna-postgres

#### GET /log

```
$ curl http://localhost:3333/log --user root:root
```

#### POST /log/reset

```
$ curl http://localhost:3333/log/reset -X POST --user root:root
```

#### GET /nodes

```
$ curl http://localhost:3333/nodes --user root:root 
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
$ curl http://localhost:3333/sp --user root:root
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
