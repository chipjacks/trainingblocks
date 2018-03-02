module Models exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import RemoteData exposing (WebData)


type alias Model =
    { activities : Dict RataDie (WebData (List HumanAPIActivity))
    , blocks : List Block
    , user : Maybe User
    }


type alias RataDie =
    Int

type alias Day = 
    { date : Date
    , blocks : List Activity
    }

-- Not persisted, just used for loading from API
-- To persist, transform and/or attach to an Activity
type alias HumanAPIActivity =
    { -- id : String,
      date : Date
    , duration : Int
    , distance : Float
    }

-- All Blocks persisted to AWS
type Block
    = ActivityBlock Activity
    | ContainerBlock Container

type alias Activity =
    { -- id : BlockId
--    , containerId: BlockId
--    , type_ : ActivityType,
     intensity : Int -- 1 to 5, either inferred from pace or user defined
    , durationMinutes : Int -- in minutes... TODO: change it to seconds.
--    , externalId: Maybe String -- if user attaches an activity
--    , completed: Bool -- could be completed without necessarily having an external activity
--    , notes: List Note
    }

type alias Container = 
    { -- id: BlockId
 --   , userId: UserId,
     blocks: WebData (List Block)
--    , scale: Scale
    , date: Date
    -- Maybe add some aggregated metrics here so recursive load can happen lazily
    }

-- plan has many week blocks, a day has many session blocks, a session has many unit blocks
-- Copy a plan = Deep copy plan container but modify dates
-- TODO: Multiple plans for the same date range?
  -- No, they would interfere with each other
  -- Yes, calendar view functions responsible for combining them
  -- IDK
type Scale = Cycle | Week -- | Day | Session
type alias BlockId = String
type ActivityType = Run | Ride | Weights | Swim

{- Add a symbol and note to an activity
    Examples:
    - 😀 Felt wonderful!
    - ⛰ Hilly!
    - 😩 So tired
    - 😷 Getting sick
    - 🥇 Winner!
    - 🏁 Race Day
-}

type alias Note =
    { label: String -- probably an emoji
    , message: String
    }

type alias User = 
    { userId : UserId
    , externalId : Maybe String -- If user attaches an Oauth account
    }

type alias UserId = String

{- TODO: Init process:
- [ ] Authenticate user via Amazon Web Identity Federation
- [ ] Load user record containing associated external Oauth token
- [ ] Recursively load blocks from Amazon DynamoDB
- [ ] Load external activities into seperate data structure, but only display ones that don't have a block
-}