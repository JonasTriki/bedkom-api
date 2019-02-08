{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module InformaticsStudy where

import qualified Data.Text.Lazy as L

import           Control.Lens

type Year = Int

type ProgramStr = L.Text

type SemesterStr = L.Text

data Semester
  = Autumn Year
  | Spring Year
  deriving (Eq)

instance Show Semester where
  show (Autumn year) = "Vår " ++ show year
  show (Spring year) = "Høst " ++ show year

strYearToSemester :: SemesterStr -> Year -> Semester
strYearToSemester semesterStr year =
  if semesterStr == "H\216ST"
    then Spring year
    else Autumn year

data Program
  = BAMN_DTEK
  | BAMN_DSIK
  | BAMN_DVIT
  | BAMN_BINF
  | BATF_IMOE
  | BASV_IKT
  | MAMN_INF
  | MAMN_PROG
  | Unknown
  deriving (Eq)

strToProgram :: ProgramStr -> Program
strToProgram "BAMN-DTEK" = BAMN_DTEK
strToProgram "BAMN-DSIK" = BAMN_DSIK
strToProgram "BAMN-DVIT" = BAMN_DVIT
strToProgram "BAMN-BINF" = BAMN_BINF
strToProgram "BATF-IMOE" = BATF_IMOE
strToProgram "BASV-IKT"  = BASV_IKT
strToProgram "MAMN-INF"  = MAMN_INF
strToProgram "MAMN-PROG" = MAMN_PROG
strToProgram _           = Unknown

instance Show Program where
  show BAMN_DTEK = "Bachelorprogram i informatikk: datateknologi"
  show BAMN_DSIK = "Bachelorprogram i informatikk: datasikkerhet"
  show BAMN_DVIT = "Bachelorprogram i informatikk: datavitenskap"
  show BAMN_BINF = "Bachelorprogram i informatikk: bioinformatikk"
  show BATF_IMOE = "Bachelorprogram i informatikk-matematikk-økonomi"
  show BASV_IKT = "Bachelorprogram i informasjons- og kommunikasjonsteknologi"
  show MAMN_INF = "Masterprogram i informatikk"
  show MAMN_PROG = "Felles masterprogram i programutvikling"
  show Unknown = "Ukjent"

data InformaticsStudy = InformaticsStudy
  { _semester :: Semester
  , _program  :: Program
  } deriving (Show, Eq)

makeLenses ''InformaticsStudy

parseProgram :: (ProgramStr, Year, SemesterStr) -> InformaticsStudy
parseProgram (programStr, year, semesterStr) =
  InformaticsStudy {_semester = strYearToSemester semesterStr year, _program = strToProgram programStr}

isInformaticsStudent :: [InformaticsStudy] -> Bool
isInformaticsStudent = foldr (\i -> (||) (view program i /= Unknown)) False
