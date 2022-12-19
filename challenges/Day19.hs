module Main where

import Advent (challenge)
import Utils (parseL, int)
import Text.Parsec hiding (parse)
import Control.Monad (guard)
import qualified Data.Set as S

data Store = MkStore { _ore :: Int, _clay :: Int, _obsidian :: Int, _geode :: Int } deriving (Eq, Ord)
data Blueprint = MkBlueprint { _bore :: Int, _bclay :: Int, _bobsidian :: (Int, Int), _bgeode :: (Int, Int) } deriving (Show)

type Challenge = [(Int, Blueprint)]

notDigit :: Parsec String () String
notDigit = many (letter <|> char ' ' <|> char ':' <|> char '.')

numberWithDesc :: Parsec String () Int
numberWithDesc = notDigit *> int

blueprint :: Parsec String () (Int, Blueprint)
blueprint = (,) <$> blueprintId <*> robots <* notDigit
  where blueprintId = numberWithDesc
        robots = MkBlueprint <$> numberWithDesc
                             <*> numberWithDesc
                             <*> ((,) <$> numberWithDesc <*> numberWithDesc)
                             <*> ((,) <$> numberWithDesc <*> numberWithDesc)

parse :: String -> Challenge
parse = parseL blueprint

mine
  :: Store -- | ore, clay, obsidian, geode resource
  -> Store -- | ore, clay, obsidian, geode robots
  -> Store -- | ore, clay, obsidian, geode resources with newly mined material
mine (MkStore ore clay obsidian geode) (MkStore oreBots clayBots obsidianBots geodeBots) =
  MkStore (ore + oreBots) (clay + clayBots) (obsidian + obsidianBots) (geode + geodeBots)

necessaryBots :: Blueprint -> Store -> (Bool, Bool, Bool)
necessaryBots (MkBlueprint o1 o2 (o3, clay) (o4, obs)) (MkStore oreBot clayBot obsBot _) =
  (oreBot < maximum [o1, o2, o3, o4], clayBot < clay, obsBot < obs)

prune :: [a] -> [a] -> [a]
[] `prune` rest = rest
some `prune` _ = some

manufacture
  :: Blueprint
  -> Store
  -> Store
  -> [(Store, Store)]
manufacture blue@(MkBlueprint oreCost clayCost obsidianCost geodeCost) resources bots = geodeBot <> obsidianBot <> clayBot <> oreBot
  where
    r = mine resources bots
    (oreBotNeeded, clayBotNeeded, obsBotNeeded) = necessaryBots blue bots
    geodeBot =
      if _ore resources >= (fst geodeCost) && _obsidian resources >= snd geodeCost
      then [(r { _ore = _ore r - fst geodeCost, _obsidian = _obsidian r - snd geodeCost }, bots { _geode = _geode bots + 1})]
      else []
    obsidianBot =
      if obsBotNeeded && _ore resources >= (fst obsidianCost) && _clay resources >= snd obsidianCost
      then [(r { _ore = _ore r - fst obsidianCost, _clay = _clay r - snd obsidianCost }, bots { _obsidian = _obsidian bots + 1})]
      else []
    clayBot =
      if clayBotNeeded && _ore resources >= clayCost
      then [(r { _ore = _ore r - clayCost }, bots { _clay = _clay bots + 1 })]
      else []
    oreBot =
      if oreBotNeeded && _ore resources >= oreCost
      then [(r { _ore = _ore r - oreCost }, bots { _ore = _ore bots + 1 })]
      else []

step
  :: [(Store, Store)]
  -> Int
  -> S.Set Store
  -> Blueprint
  -> [Store]
step frontier  0    _    _         = map fst frontier
step frontier time seen blueprint = step next (time - 1) (S.union seen (S.fromList $ map snd next)) blueprint
  where
    mines = map (\(r, b) -> (mine r b, b)) frontier
    next = mines ++ do
      (resources, bots) <- frontier
      (resources', bots') <- manufacture blueprint resources bots
      guard $ S.notMember bots' seen
      pure (resources', bots')

runBlueprint :: Int -> Blueprint -> Int
runBlueprint time = maximum . map _geode . step [(MkStore 0 0 0 0, MkStore 1 0 0 0)] time (S.singleton $ MkStore 0 0 0 0)

part1 :: Challenge -> String
part1 = show . sum . map (\(i, blueprint) -> i * runBlueprint 24 blueprint)

part2 :: Challenge -> String
part2 = show . product . map (runBlueprint 32 . snd) . take 3 

main :: IO ()
main = challenge 19 parse part1 part2