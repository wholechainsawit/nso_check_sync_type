# vi ra-*- mode: python; python-indent: 4 -*-
import ncs
from ncs.dp import Action

class CheckSyncAction(Action):
    @Action.action
    def cb_action(self, uinfo, name, kp, input, output, trans):
        pass


class Main(ncs.application.Application):
    def setup(self):
        self.log.info("Main RUNNING")

    def teardown(self):
        self.log.info("Main FINISHED")
