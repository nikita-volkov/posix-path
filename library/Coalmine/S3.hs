module Coalmine.S3 where

import qualified Coalmine.Fx as Fx
import Fx
import NeatInterpolation
import Prelude

data StorageClass
  = StandardStorageClass
  | ReducedRedundancyStorageClass
  | StandardIaStorageClass
  | OneZoneIaStorageClass
  | IntelligentTieringStorageClass
  | GlacierStorageClass
  | DeepArchiveStorageClass

data Acl
  = PrivateAcl
  | PublicReadAcl
  | PublicReadWriteAcl
  | AuthenticatedReadAcl
  | AwsExecReadAcl
  | BucketOwnerReadAcl
  | BucketOwnerFullControlAcl
  | LogDeliveryWriteAcl

upload :: StorageClass -> Acl -> Text -> FilePath -> Fx env (Text, Text) ()
upload storageClass acl s3Dir file = void $ Fx.runCmd cmdText
  where
    cmdText = [text|aws s3 cp --quiet --storage-class=$storageClassText --acl=$aclText $sourceText $s3Dir|]
    storageClassText = case storageClass of
      StandardStorageClass -> "STANDARD"
      ReducedRedundancyStorageClass -> "REDUCED_REDUNDANCY"
      StandardIaStorageClass -> "STANDARD_IA"
      OneZoneIaStorageClass -> "ONE-ZONE_IA"
      IntelligentTieringStorageClass -> "INTELLIGENT_TIERING"
      GlacierStorageClass -> "GLACIER"
      DeepArchiveStorageClass -> "DEEP_ARCHIVE"
    sourceText = fromString file
    aclText = case acl of
      PrivateAcl -> "private"
      PublicReadAcl -> "public-read"
      PublicReadWriteAcl -> "public-read-write"
      AuthenticatedReadAcl -> "authenticated-read"
      AwsExecReadAcl -> "aws-exec-read"
      BucketOwnerReadAcl -> "bucket-owner-read"
      BucketOwnerFullControlAcl -> "bucket-owner-full-control"
      LogDeliveryWriteAcl -> "log-delivery-write"
