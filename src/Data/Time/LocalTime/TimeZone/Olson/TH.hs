{- |
Load TimeZoneSeries from an Olson file at compile time.
-}
module Data.Time.LocalTime.TimeZone.Olson.TH 
  (
    loadTZ
  ) where       

import Data.Ratio                          (numerator,
                                            denominator)
import Data.Time.LocalTime.TimeZone.Olson  (getTimeZoneSeriesFromOlsonFile)
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(..))
import Data.Time.LocalTime                 (TimeZone(..))
import Data.Time                           (UTCTime(..),
                                            Day(..),
                                            DiffTime)
import Language.Haskell.TH                 (Q,
                                            runIO,
                                            Exp(..),
                                            mkName,
                                            Lit(..))

-- | Make a splice of a TimeZoneSeries from an Olson file.
--   
--   For example:
--   > myTimeZoneSeries :: TimeZoneSeries
--   > myTimeZoneSeries = $(loadTZ "/usr/share/zoneinfo/Europe/Stockholm")
loadTZ :: FilePath -- ^ Path to the Olson file.
       -> Q Exp
loadTZ zf = runIO $                                                         
         do
    TimeZoneSeries def trans <- getTimeZoneSeriesFromOlsonFile zf
    return $ mkTZS (def,trans)
    
mkTZS :: (TimeZone,[(UTCTime,TimeZone)]) -> Exp    
mkTZS (def,tlist) =    
  AppE (AppE (ConE $ mkName "TimeZoneSeries") (litTimeZone def))
  (mkList tlist)
  
mkList :: [(UTCTime,TimeZone)] -> Exp  
mkList l = ListE $ map mkPair l    
    
mkPair :: (UTCTime,TimeZone) -> Exp           
mkPair (t,tz) =    
  TupE [litUTCTime t,litTimeZone tz]
    
litUTCTime :: UTCTime -> Exp  
litUTCTime (UTCTime (ModifiedJulianDay d) s) = 
  AppE (AppE (ConE $ mkName "UTCTime")
        (AppE (ConE (mkName "ModifiedJulianDay")) (LitE $ IntegerL $ d)))
  (AppE (VarE $ mkName "secondsToDiffTime") 
   (LitE $ IntegerL $ diffTimeToInteger s))

diffTimeToInteger :: DiffTime -> Integer
diffTimeToInteger s =
  let r = toRational s
      n = numerator r
      d = denominator r in
  (n `div` d)

litTimeZone :: TimeZone -> Exp
litTimeZone (TimeZone m s n) = 
  AppE (AppE (AppE (ConE (mkName "TimeZone")) 
              (LitE $ IntegerL $ toInteger m))
        (ConE $ mkName $ show s))
  (LitE $ StringL n)
        