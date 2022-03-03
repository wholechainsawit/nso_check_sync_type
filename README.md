## To get all services call points

```
admin@ncs# services get-services
service {
    path /ncs:services/zone-info:zone-info
    
}
service {
    path /ncs:services/nms:nms
    
}
ervice {
    path /cross-vnf-info:cross-vnf-info
}
```

## Sample RESTCONF call

### Get all services path

`curl -i -u 'admin:admin' -X POST -H 'Content-Type:application/yang-data+json' -H 'Accept:application/yang-data+json' 'http://localhost:25080/restconf/data/tailf-ncs:services/check_sync:get-services'`

