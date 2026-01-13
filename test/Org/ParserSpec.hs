{-# LANGUAGE OverloadedStrings #-}

module Org.ParserSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Org.Types (OrgFile (..), OrgHeadline (..))
import qualified Org.Parser as Parser

spec :: Spec
spec = do
  describe "Org.Parser" $ do
    it "parses a single headline" $ do
      let input = "* Hello"
      Parser.parseOrgText input
        `shouldBe`
          Right
            ( OrgFile
                { orgFileHeadlines =
                    [ OrgHeadline
                        { headlineId = "hello"
                        , headlineLevel = 1
                        , headlineTitle = "Hello"
                        , headlineTodo = Nothing
                        , headlineTags = []
                        , headlineScheduled = Nothing
                        , headlineProperties = Map.empty
                        , headlineBody = []
                        , headlineChildren = []
                        }
                    ]
                }
            )

    it "parses TODO and tags" $ do
      let input = "* TODO Hello :work:personal:"
      Parser.parseOrgText input
        `shouldBe`
          Right
            ( OrgFile
                { orgFileHeadlines =
                    [ OrgHeadline
                        { headlineId = "hello"
                        , headlineLevel = 1
                        , headlineTitle = "Hello"
                        , headlineTodo = Just "TODO"
                        , headlineTags = ["work", "personal"]
                        , headlineScheduled = Nothing
                        , headlineProperties = Map.empty
                        , headlineBody = []
                        , headlineChildren = []
                        }
                    ]
                }
            )

    it "parses mixed-case TODO keywords" $ do
      let input = "* Next Hello"
      Parser.parseOrgText input
        `shouldBe`
          Right
            ( OrgFile
                { orgFileHeadlines =
                    [ OrgHeadline
                        { headlineId = "hello"
                        , headlineLevel = 1
                        , headlineTitle = "Hello"
                        , headlineTodo = Just "Next"
                        , headlineTags = []
                        , headlineScheduled = Nothing
                        , headlineProperties = Map.empty
                        , headlineBody = []
                        , headlineChildren = []
                        }
                    ]
                }
            )

    it "parses scheduled timestamps" $ do
      let input = "* Hello\nSCHEDULED: <2024-01-01 Mon>"
      Parser.parseOrgText input
        `shouldBe`
          Right
            ( OrgFile
                { orgFileHeadlines =
                    [ OrgHeadline
                        { headlineId = "hello"
                        , headlineLevel = 1
                        , headlineTitle = "Hello"
                        , headlineTodo = Nothing
                        , headlineTags = []
                        , headlineScheduled = Just "2024-01-01 Mon"
                        , headlineProperties = Map.empty
                        , headlineBody = []
                        , headlineChildren = []
                        }
                    ]
                }
            )

    it "parses property drawers" $ do
      let input = "* Hello\n:PROPERTIES:\n:ID: 123\n:END:"
      Parser.parseOrgText input
        `shouldBe`
          Right
            ( OrgFile
                { orgFileHeadlines =
                    [ OrgHeadline
                        { headlineId = "hello"
                        , headlineLevel = 1
                        , headlineTitle = "Hello"
                        , headlineTodo = Nothing
                        , headlineTags = []
                        , headlineScheduled = Nothing
                        , headlineProperties = Map.fromList [("ID", "123")]
                        , headlineBody = []
                        , headlineChildren = []
                        }
                    ]
                }
            )

    it "keeps the last property value when duplicated" $ do
      let input = "* Hello\n:PROPERTIES:\n:ID: 1\n:ID: 2\n:END:"
      Parser.parseOrgText input
        `shouldBe`
          Right
            ( OrgFile
                { orgFileHeadlines =
                    [ OrgHeadline
                        { headlineId = "hello"
                        , headlineLevel = 1
                        , headlineTitle = "Hello"
                        , headlineTodo = Nothing
                        , headlineTags = []
                        , headlineScheduled = Nothing
                        , headlineProperties = Map.fromList [("ID", "2")]
                        , headlineBody = []
                        , headlineChildren = []
                        }
                    ]
                }
            )

    it "parses body lines" $ do
      let input = "* Hello\nLine one\nLine two"
      Parser.parseOrgText input
        `shouldBe`
          Right
            ( OrgFile
                { orgFileHeadlines =
                    [ OrgHeadline
                        { headlineId = "hello"
                        , headlineLevel = 1
                        , headlineTitle = "Hello"
                        , headlineTodo = Nothing
                        , headlineTags = []
                        , headlineScheduled = Nothing
                        , headlineProperties = Map.empty
                        , headlineBody = ["Line one", "Line two"]
                        , headlineChildren = []
                        }
                    ]
                }
            )

    it "parses child headlines" $ do
      let input = "* Parent\n** Child"
      Parser.parseOrgText input
        `shouldBe`
          Right
            ( OrgFile
                { orgFileHeadlines =
                    [ OrgHeadline
                        { headlineId = "parent"
                        , headlineLevel = 1
                        , headlineTitle = "Parent"
                        , headlineTodo = Nothing
                        , headlineTags = []
                        , headlineScheduled = Nothing
                        , headlineProperties = Map.empty
                        , headlineBody = []
                        , headlineChildren =
                            [ OrgHeadline
                                { headlineId = "child"
                                , headlineLevel = 2
                                , headlineTitle = "Child"
                                , headlineTodo = Nothing
                                , headlineTags = []
                                , headlineScheduled = Nothing
                                , headlineProperties = Map.empty
                                , headlineBody = []
                                , headlineChildren = []
                                }
                            ]
                        }
                    ]
                }
            )

    it "fails on invalid headline" $ do
      let input = "*Invalid"
      Parser.parseOrgText input `shouldSatisfy` isLeft
