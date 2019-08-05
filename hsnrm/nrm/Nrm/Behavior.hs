{-|
Module      : Nrm.Behavior
Description : Nrm Behavior
Copyright   : (c) UChicago Argonne, 2019
License     : BSD3
Maintainer  : fre@freux.fr
-}
module Nrm.Behavior
  (behave
  )
where

import Protolude
import Nrm.Types.Configuration
import Nrm.Types.Application
{-import Nrm.Types.Container-}

data Behavior = NoBehavior | Reply ByteString | StartChild Command Arguments

data Event = Upstream ByteString | Downstream ByteString

behave :: Cfg -> State -> Event -> (State, Behavior)
behave Cfg {..} (Upstream s) = upstreamReceive s
behave Cfg {..} (Downstream s) = downstreamReceive s

downstreamReceive :: NrmState -> ByteString -> (NrmState , Behavior)
downstreamreceive nrmState s = (nrmState, NoBehavior)
        {-logger.info("receiving downstream message: %r", event)-}
        {-if event.tag == 'start':-}
            {-cid = event.container_uuid-}
            {-container = self.container_manager.containers[cid]-}
            {-self.application_manager.register(event, container)-}
        {-elif event.tag == 'progress':-}
            {-if event.application_uuid in self.application_manager.applications:-}
                {-app = self.application_manager.applications[-}
                    {-event.application_uuid]-}
                {-app.update_performance(event)-}
                {-# self.upstream_pub_server.send(event) TODO try this.-}
                {-self.upstream_pub_server.send(-}
                    {-tag='progress',-}
                    {-payload=event.payload,-}
                    {-application_uuid=event.application_uuid)-}
        {-elif event.tag == 'performance':-}
            {-if event.application_uuid in self.application_manager.applications:-}
                {-app = self.application_manager.applications[-}
                    {-event.application_uuid]-}
                {-app.update_performance(event)-}
            {-self.upstream_pub_server.send(-}
                {-tag='performance',-}
                {-payload=event.payload,-}
                {-container_uuid=event.container_uuid)-}
        {-elif event.tag == 'phasecontext':-}
            {-uuid = event.application_uuid-}
            {-if uuid in self.application_manager.applications:-}
                {-app = self.application_manager.applications[uuid]-}
                {-if bool(self.container_manager.containers):-}
                    {-cid = app.container_uuid-}
                    {-c = self.container_manager.containers[cid]-}
                    {-if c.power['policy']:-}
                        {-app.update_phase_context(event)-}
                        {-# Run container policy-}
                        {-self.controller.run_policy_container(c, app)-}
        {-elif event.tag == 'exit':-}
            {-uuid = event.application_uuid-}
            {-if uuid in self.application_manager.applications:-}
                {-self.application_manager.delete(uuid)-}
        {-else:-}
            {-logger.error("unknown msg: %r", event)-}
            {-return-}
