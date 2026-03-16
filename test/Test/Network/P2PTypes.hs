module Test.Network.P2PTypes (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Network.P2P.Types

tests :: TestTree
tests = testGroup "Network.P2P.Types"
  [ testCase "topicString for attestation subnet" $
      topicString (TopicAttestation 2) @?= "attestation_2"

  , testCase "topicString for aggregation" $
      topicString TopicAggregation @?= "aggregation"

  , testCase "topicString for beacon block" $
      topicString TopicBeaconBlock @?= "beacon_block"

  , testCase "topics are orderable" $ do
      let t1 = TopicAttestation 0
          t2 = TopicAttestation 1
          t3 = TopicAggregation
          t4 = TopicBeaconBlock
      assertBool "attestation topics orderable" (t1 < t2)
      assertBool "different topic types orderable" (t3 < t4 || t4 < t3 || t3 == t4)

  , testCase "P2PConfig show" $ do
      let cfg = P2PConfig 9000 ["/ip4/127.0.0.1/tcp/9001"]
      length (show cfg) > 0 @? "P2PConfig should be showable"
  ]
