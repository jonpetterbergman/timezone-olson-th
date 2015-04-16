{-# LANGUAGE TemplateHaskell #-}
-- |
-- Load TimeZoneSeries from an Olson file at compile time.
--  
-- For example:
--
-- > myTimeZoneSeries :: TimeZoneSeries
-- > myTimeZoneSeries = $(loadTZFile "/usr/share/zoneinfo/Europe/Stockholm")

module Data.Time.LocalTime.TimeZone.Olson.TH 
  (
    loadTZFile
  ) where       

import Data.Ratio                          (numerator,
                                            denominator)
import Data.Time.LocalTime.TimeZone.Olson  (getTimeZoneSeriesFromOlsonFile)
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(..))
import Data.Time.LocalTime                 (TimeZone(..))
import Data.Time                           (UTCTime(..),
                                            Day(..),
                                            DiffTime,
                                            secondsToDiffTime)
import Language.Haskell.TH                 (Q,
                                            runIO,
                                            Exp(..),
                                            mkName,
                                            Lit(..))

-- | Make a splice of a TimeZoneSeries from an Olson file.
loadTZFile :: FilePath -- ^ Path to the Olson file.
           -> Q Exp
loadTZFile zf = 
  runIO $ fmap mkTZS $ getTimeZoneSeriesFromOlsonFile zf

-- | Make a splice of a TimeZoneSeries.    
mkTZS :: TimeZoneSeries -- ^ The TimeZoneSeries to be spliced
      -> Exp    
mkTZS (TimeZoneSeries def tlist) =    
  AppE (AppE (ConE 'TimeZoneSeries) (litTimeZone def))
  (mkList tlist)
  
mkList :: [(UTCTime,TimeZone)] 
       -> Exp  
mkList l = ListE $ map mkPair l    
    
mkPair :: (UTCTime,TimeZone) 
       -> Exp           
mkPair (t,tz) =    
  TupE [litUTCTime t,litTimeZone tz]
    
litUTCTime :: UTCTime 
           -> Exp  
litUTCTime (UTCTime (ModifiedJulianDay d) s) = 
  AppE (AppE (ConE 'UTCTime)
        (AppE (ConE 'ModifiedJulianDay) (LitE $ IntegerL $ d)))
  (AppE (VarE 'secondsToDiffTime) 
   (LitE $ IntegerL $ diffTimeToInteger s))

diffTimeToInteger :: DiffTime 
                  -> Integer
diffTimeToInteger s =
  let r = toRational s
      n = numerator r
      d = denominator r in
  (n `div` d)

litTimeZone :: TimeZone 
            -> Exp
litTimeZone (TimeZone m s n) = 
  AppE (AppE (AppE (ConE 'TimeZone) 
              (LitE $ IntegerL $ toInteger m))
        (ConE $ mkName $ show s))
  (LitE $ StringL n)
        