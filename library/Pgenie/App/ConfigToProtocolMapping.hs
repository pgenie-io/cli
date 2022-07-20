module Pgenie.App.ConfigToProtocolMapping where

import Coalmine.Prelude
import qualified Pgenie.Config.Model as Config
import qualified Pgenie.Protocol.V1 as Protocol

artifacts :: Config.ProjectArtifacts -> BVec Protocol.Artifact
artifacts config =
  mconcat
    [ elem #haskellHasql Protocol.HaskellHasqlArtifact,
      elem #javaJdbc Protocol.JavaJdbcArtifact
    ]
  where
    elem selector artifact =
      if selector config then pure artifact else mempty
