# vi ra-*- mode: python; python-indent: 4 -*-
import ncs
from ncs.dp import Action


def do_check_sync(service_instance, outformat):
    arguments = service_instance.check_sync.get_input()
    arguments.outformat = outformat
    result = service_instance.check_sync(arguments)
    return result


def get_services(root):
    result = []
    services = root.services.get_services()
    for s in services.service:
        result.append(s.path)
    return result


class CheckSyncAction(Action):
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        outformat = input.outformat
        services_type = input.services
        for service_type in services_type:
            # suppose only the name of the service is given
            service_full_path = "/ncs:services/" + service_type + ":" + service_type

            root = ncs.maagic.get_root(trans)
            service_types = get_services(root)

            if service_full_path in service_types:
                srvc_instances = ncs.maagic.cd(root, service_full_path)
                for srvc_instance in srvc_instances:
                    sync_result = output.sync_result.create()
                    sync_result.service_id = srvc_instance._path
                    try:
                        result = do_check_sync(srvc_instance, outformat)
                    except Exception as nso_error:
                        sync_result.in_sync = "error"
                        sync_result.error = str(nso_error)
                    else:
                        if outformat == "native":
                            sync_result.in_sync = False if result.native.device else True
                            for d in result.native.device:
                                rd = sync_result.native.device.create(d.name)
                                rd.data = d.data
                        elif outformat == "cli":
                            sync_result.in_sync = False if result.cli.local_node.data else True
                            sync_result.cli.data = result.cli.local_node.data
                        elif outformat == "xml":
                            sync_result.in_sync = False if result.result_xml.local_node.data else True
                            sync_result.result_xml.data = result.result_xml.local_node.data
                        elif outformat == "boolean":
                            sync_result.in_sync = result.in_sync
                        else:
                            output.error = "No such outformat, {}".format(outformat)
            else:
                output.error = "Unknown service type '{}'".format(service_full_path)


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("Main RUNNING")
        self.register_action("check-sync", CheckSyncAction)

    def teardown(self):
        self.log.info("Main FINISHED")
