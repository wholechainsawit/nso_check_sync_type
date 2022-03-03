## Installation

1. In the `check-sync-type` directory, type `make` to compile the action package
2. Reload as usual NSO service package


## To check only a certain type of service

For example,
`admin@ncs# services check-sync-type service nms outformat native`

the output is similar to the regular check-sync action
`admin@ncs# services nms * check-sync outformat native`


The `check-sync-type` also support different outformats
e.g.
`admin@ncs# services check-sync-type service nms outformat cli`
`admin@ncs# services check-sync-type service nms outformat xml`


## To get all services call points

```
admin@ncs# services get-services
service {
    path /ncs:services/zone-info:zone-info
    
}
service {
    path /ncs:services/spaas:spaas
    
}
service {
    path /ncs:services/nms:nms
    
}

```

## Sample RESTCONF call

### Get all services path

`curl -i -u 'admin:admin' -X POST -H 'Content-Type:application/yang-data+json' -H 'Accept:application/yang-data+json' 'http://localhost:25080/restconf/data/tailf-ncs:services/check_sync:get-services'`


### Check-sync only on nms services

`curl -i -u 'admin:admin' -X POST -H 'Content-Type:application/yang-data+json' -H 'Accept:application/yang-data+json' 'http://localhost:25080/restconf/data/tailf-ncs:services/check_sync:check-sync-type' -d '{ "input": { "service": "nms" } }'`

