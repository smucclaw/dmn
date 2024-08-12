{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows #-}

module FromXML where

import Text.XML.HXT.Core hiding (Schema)
import Data.Maybe (fromMaybe)
import Types

parseDecision :: IOSArrow XmlTree Decision
parseDecision = proc x -> do
    decisionName <- getAttrValue "id" -< x
    decisionOut <- getChildren >>> hasName "decisionTable" >>> parseDecOutVar -< x
    decisionInfoReq <- listA (getChildren >>> hasName "informationRequirement" >>> parseInfoReq) -< x
    decisionLogic <- getChildren >>> hasName "decisionTable" >>> parseDecLogic -< x
    returnA -< Decision {..}

parseDecOutVar :: IOSArrow XmlTree DecOutVar
parseDecOutVar = proc x -> do
    sDecVarName <- getChildren >>> hasName "output" >>> getAttrValue "name" -< x
    sDecVarFEELType <- getChildren >>> hasName "output" >>> getAttrValue "typeRef" -< x
    returnA -< DecOutVar {..}

parseInfoReq :: IOSArrow XmlTree InfoReq
parseInfoReq = proc x -> do
    sReqInput <- getAttrValue "id" -< x
    returnA -< ReqInputEl {..}

parseDecLogic :: IOSArrow XmlTree DecLogic
parseDecLogic = proc x -> do
    decTableId <- getAttrValue "id" -< x
    hitPolicy <- getAttrValue "hitPolicy" -< x
    schema <- getChildren >>> hasName "input" >>> parseSchema -< x
    rules <- listA (getChildren >>> hasName "rule" >>> parseRule) -< x
    returnA -< DecTable {..}

parseSchema :: IOSArrow XmlTree Schema
parseSchema = proc x -> do
    sInputSchemas <- listA (getChildren >>> hasName "input" >>> parseInputSchema) -< x
    sOutputSchema <- getChildren >>> hasName "output" >>> parseOutputSchema -< x
    returnA -< Schema {..}

parseInputSchema :: IOSArrow XmlTree InputSchema
parseInputSchema = proc x -> do
    sInputSchemaId <- getAttrValue "label" -< x
    inputExprFEELType <- getAttrValue "typeRef" -< x
    returnA -< InputSchema {..}

parseOutputSchema :: IOSArrow XmlTree OutputSchema
parseOutputSchema = proc x -> do
    sOutputSchemaVarName <- getAttrValue "name" -< x
    sOutputSchemaFEELType <- getAttrValue "typeRef" -< x
    returnA -< OutputSchema {..}

parseRule :: IOSArrow XmlTree Rule
parseRule = proc x -> do
    ruleId <- getAttrValue "id" -< x
    inputEntries <- listA (getChildren >>> hasName "inputEntry" >>> parseInputEntry) -< x
    outputEntry <- getChildren >>> hasName "outputEntry" >>> parseOutputEntry -< x
    returnA -< Rule {..}

parseInputEntry :: IOSArrow XmlTree InputEntry
parseInputEntry = proc x -> do
    sInputEntryId <- getAttrValue "id" -< x
    sMaybeCondition <- getChildren >>> hasName "text" >>> parseCondition -< x
    returnA -< InputEntry {..}

parseOutputEntry :: IOSArrow XmlTree OutputEntry
parseOutputEntry = proc x -> do
    sOutputId <- getAttrValue "id" -< x
    sExpr <- getChildren >>> hasName "text" >>> getText -< x
    returnA -< OutputEntry {..}

parseCondition :: IOSArrow XmlTree (Maybe Condition)
parseCondition = proc x -> do
    sCondition <- getText -< x
    returnA -< case sCondition of
        "" -> Nothing
        _ -> Just $ ConditionString sCondition