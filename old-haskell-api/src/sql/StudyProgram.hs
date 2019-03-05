{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module StudyProgram where

import           Database.Persist.TH

data StudyProgram
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

instance Read StudyProgram where
  readsPrec _ =
    \case
      "BAMN-DTEK" -> [(BAMN_DTEK, "")]
      "BAMN-DSIK" -> [(BAMN_DSIK, "")]
      "BAMN-DVIT" -> [(BAMN_DVIT, "")]
      "BAMN-BINF" -> [(BAMN_BINF, "")]
      "BATF-IMOE" -> [(BATF_IMOE, "")]
      "BASV-IKT" -> [(BASV_IKT, "")]
      "MAMN-INF" -> [(MAMN_INF, "")]
      "MAMN-PROG" -> [(MAMN_PROG, "")]
      _ -> [(Unknown, "")]

instance Show StudyProgram where
  show BAMN_DTEK = "Bachelorprogram i informatikk: datateknologi"
  show BAMN_DSIK = "Bachelorprogram i informatikk: datasikkerhet"
  show BAMN_DVIT = "Bachelorprogram i informatikk: datavitenskap"
  show BAMN_BINF = "Bachelorprogram i informatikk: bioinformatikk"
  show BATF_IMOE = "Bachelorprogram i informatikk-matematikk-økonomi"
  show BASV_IKT = "Bachelorprogram i informasjons- og kommunikasjonsteknologi"
  show MAMN_INF = "Masterprogram i informatikk"
  show MAMN_PROG = "Felles masterprogram i programutvikling"
  show Unknown = "Ukjent"

derivePersistField "StudyProgram"
