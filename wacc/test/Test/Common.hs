module Test.Common where

validTests :: [FilePath]
validTests =
  [ "valid/advanced/binarySortTree.wacc"
  , "valid/advanced/ticTacToe.wacc"
  , "valid/advanced/hashTable.wacc"
  , "valid/array/arrayLength.wacc"
  , "valid/array/lenArrayIndex.wacc"
  , "valid/array/arrayIndexMayBeArrayIndex.wacc"
  , "valid/array/emptyArrayReplace.wacc"
  , "valid/array/stringFromArray.wacc"
  , "valid/array/arrayEmpty.wacc"
  , "valid/array/array.wacc"
  , "valid/array/arrayLookup.wacc"
  , "valid/array/emptyArrayPrint.wacc"
  , "valid/array/free.wacc"
  , "valid/array/arrayOnHeap.wacc"
  , "valid/array/modifyString.wacc"
  , "valid/array/printRef.wacc"
  , "valid/array/emptyArrayNextLine.wacc"
  , "valid/array/arraySimple.wacc"
  , "valid/array/charArrayInStringArray.wacc"
  , "valid/array/arrayNested.wacc"
  , "valid/array/arrayPrint.wacc"
  , "valid/array/emptyArrayScope.wacc"
  , "valid/array/arrayBasic.wacc"
  , "valid/array/emptyArrayAloneIsFine.wacc"
  , "valid/pairs/printNull.wacc"
  , "valid/pairs/printNullPair.wacc"
  , "valid/pairs/printPair.wacc"
  , "valid/pairs/nestedPair.wacc"
  , "valid/pairs/createRefPair.wacc"
  , "valid/pairs/free.wacc"
  , "valid/pairs/pairExchangeArrayOk.wacc"
  , "valid/pairs/writeSnd.wacc"
  , "valid/pairs/nestedPairLeftAssign.wacc"
  , "valid/pairs/writeFst.wacc"
  , "valid/pairs/printPairOfNulls.wacc"
  , "valid/pairs/null.wacc"
  , "valid/pairs/pairarray.wacc"
  , "valid/pairs/createPair02.wacc"
  , "valid/pairs/createPair.wacc"
  , "valid/pairs/nestedPairRightExtract.wacc"
  , "valid/pairs/checkRefPair.wacc"
  , "valid/pairs/createPair03.wacc"
  , "valid/pairs/readPair.wacc"
  , "valid/pairs/linkedList.wacc"
  , "valid/if/if1.wacc"
  , "valid/if/whitespace.wacc"
  , "valid/if/ifFalse.wacc"
  , "valid/if/ifBasic.wacc"
  , "valid/if/if6.wacc"
  , "valid/if/ifTrue.wacc"
  , "valid/if/if5.wacc"
  , "valid/if/if4.wacc"
  , "valid/if/if3.wacc"
  , "valid/if/if2.wacc"
  , "valid/runtimeErr/integerOverflow/intJustOverflow.wacc"
  , "valid/runtimeErr/integerOverflow/intnegateOverflow.wacc"
  , "valid/runtimeErr/integerOverflow/intmultOverflow.wacc"
  , "valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc"
  , "valid/runtimeErr/integerOverflow/intWayOverflow.wacc"
  , "valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc"
  , "valid/runtimeErr/integerOverflow/intUnderflow.wacc"
  , "valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc"
  , "valid/runtimeErr/nullDereference/readNull2.wacc"
  , "valid/runtimeErr/nullDereference/useNull2.wacc"
  , "valid/runtimeErr/nullDereference/setNull1.wacc"
  , "valid/runtimeErr/nullDereference/freeNull.wacc"
  , "valid/runtimeErr/nullDereference/setNull2.wacc"
  , "valid/runtimeErr/nullDereference/useNull1.wacc"
  , "valid/runtimeErr/nullDereference/readNull1.wacc"
  , "valid/runtimeErr/badChar/tooBigChr.wacc"
  , "valid/runtimeErr/badChar/negativeChr.wacc"
  , "valid/runtimeErr/divideByZero/divZero.wacc"
  , "valid/runtimeErr/divideByZero/divideByZero.wacc"
  , "valid/runtimeErr/divideByZero/modByZero.wacc"
  , "valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc"
  , "valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc"
  , "valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc"
  , "valid/IO/IOLoop.wacc"
  , "valid/IO/read/echoPuncChar.wacc"
  , "valid/IO/read/echoBigInt.wacc"
  , "valid/IO/read/echoInt.wacc"
  , "valid/IO/read/echoNegInt.wacc"
  , "valid/IO/read/echoChar.wacc"
  , "valid/IO/read/echoBigNegInt.wacc"
  , "valid/IO/read/readAtEof.wacc"
  , "valid/IO/read/read.wacc"
  , "valid/IO/IOSequence.wacc"
  , "valid/IO/print/printCharAsString.wacc"
  , "valid/IO/print/println.wacc"
  , "valid/IO/print/printEscChar.wacc"
  , "valid/IO/print/multipleStringsAssignment.wacc"
  , "valid/IO/print/print.wacc"
  , "valid/IO/print/hashInProgram.wacc"
  , "valid/IO/print/printBool.wacc"
  , "valid/IO/print/printCharArray.wacc"
  , "valid/IO/print/print-backspace.wacc"
  , "valid/IO/print/printInt.wacc"
  , "valid/IO/print/printChar.wacc"
  , "valid/basic/skip/skip.wacc"
  , "valid/basic/skip/commentEoF.wacc"
  , "valid/basic/skip/comment.wacc"
  , "valid/basic/skip/commentInLine.wacc"
  , "valid/basic/exit/exit-1.wacc"
  , "valid/basic/exit/exitBasic.wacc"
  , "valid/basic/exit/exitBasic2.wacc"
  , "valid/basic/exit/exitWrap.wacc"
  , "valid/scope/printAllTypes.wacc"
  , "valid/scope/splitScope.wacc"
  , "valid/scope/scopeSimpleRedefine.wacc"
  , "valid/scope/scopeWhileRedefine.wacc"
  , "valid/scope/scopeRedefine.wacc"
  , "valid/scope/indentationNotImportant.wacc"
  , "valid/scope/scopeIfRedefine.wacc"
  , "valid/scope/scope.wacc"
  , "valid/scope/scopeBasic.wacc"
  , "valid/scope/ifNested2.wacc"
  , "valid/scope/ifNested1.wacc"
  , "valid/scope/scopeVars.wacc"
  , "valid/scope/intsAndKeywords.wacc"
  , "valid/scope/scopeWhileNested.wacc"
  , "valid/function/nested_functions/fixedPointRealArithmetic.wacc"
  , "valid/function/nested_functions/printTriangle.wacc"
  , "valid/function/nested_functions/printInputTriangle.wacc"
  , "valid/function/nested_functions/mutualRecursion.wacc"
  , "valid/function/nested_functions/fibonacciFullRec.wacc"
  , "valid/function/nested_functions/functionConditionalReturn.wacc"
  , "valid/function/nested_functions/simpleRecursion.wacc"
  , "valid/function/nested_functions/fibonacciRecursive.wacc"
  , "valid/function/simple_functions/argScopeCanBeShadowed.wacc"
  , "valid/function/simple_functions/manyArgumentsChar.wacc"
  , "valid/function/simple_functions/functionDeclaration.wacc"
  , "valid/function/simple_functions/usesArgumentWhilstMakingArgument.wacc"
  , "valid/function/simple_functions/sameArgName2.wacc"
  , "valid/function/simple_functions/lotsOfLocals.wacc"
  , "valid/function/simple_functions/functionManyArguments.wacc"
  , "valid/function/simple_functions/punning.wacc"
  , "valid/function/simple_functions/asciiTable.wacc"
  , "valid/function/simple_functions/functionSimple.wacc"
  , "valid/function/simple_functions/incFunction.wacc"
  , "valid/function/simple_functions/sameNameAsVar.wacc"
  , "valid/function/simple_functions/functionMultiReturns.wacc"
  , "valid/function/simple_functions/sameArgName.wacc"
  , "valid/function/simple_functions/negFunction.wacc"
  , "valid/function/simple_functions/functionUpdateParameter.wacc"
  , "valid/function/simple_functions/manyArgumentsInt.wacc"
  , "valid/function/simple_functions/functionIfReturns.wacc"
  , "valid/function/simple_functions/functionReturnPair.wacc"
  , "valid/function/simple_functions/functionDoubleReturn.wacc"
  , "valid/function/simple_functions/functionSimpleLoop.wacc"
  , "valid/variables/capCharDeclaration.wacc"
  , "valid/variables/negIntDeclaration.wacc"
  , "valid/variables/charDeclaration.wacc"
  , "valid/variables/emptyStringDeclaration.wacc"
  , "valid/variables/stringDeclaration.wacc"
  , "valid/variables/longVarNames.wacc"
  , "valid/variables/boolDeclaration.wacc"
  , "valid/variables/_VarNames.wacc"
  , "valid/variables/charDeclaration2.wacc"
  , "valid/variables/manyVariables.wacc"
  , "valid/variables/intDeclaration.wacc"
  , "valid/variables/boolDeclaration2.wacc"
  , "valid/variables/zeroIntDeclaration.wacc"
  , "valid/variables/puncCharDeclaration.wacc"
  , "valid/variables/stringCarriageReturn.wacc"
  , "valid/expressions/intCalc.wacc"
  , "valid/expressions/multNoWhitespaceExpr.wacc"
  , "valid/expressions/andExpr.wacc"
  , "valid/expressions/equalsOverBool.wacc"
  , "valid/expressions/divExpr.wacc"
  , "valid/expressions/negBothMod.wacc"
  , "valid/expressions/longSplitExpr.wacc"
  , "valid/expressions/negDivisorDiv.wacc"
  , "valid/expressions/ordAndchrExpr.wacc"
  , "valid/expressions/lessExpr.wacc"
  , "valid/expressions/lessCharExpr.wacc"
  , "valid/expressions/equalsOverOr.wacc"
  , "valid/expressions/negDividendDiv.wacc"
  , "valid/expressions/equalsOverAnd.wacc"
  , "valid/expressions/longExpr2.wacc"
  , "valid/expressions/longSplitExpr2.wacc"
  , "valid/expressions/minusExpr.wacc"
  , "valid/expressions/intExpr1.wacc"
  , "valid/expressions/greaterEqExpr.wacc"
  , "valid/expressions/plusPlusExpr.wacc"
  , "valid/expressions/lessEqExpr.wacc"
  , "valid/expressions/multExpr.wacc"
  , "valid/expressions/longExpr3.wacc"
  , "valid/expressions/plusNoWhitespaceExpr.wacc"
  , "valid/expressions/plusMinusExpr.wacc"
  , "valid/expressions/negDividendMod.wacc"
  , "valid/expressions/longExpr.wacc"
  , "valid/expressions/boolExpr1.wacc"
  , "valid/expressions/equalsExpr.wacc"
  , "valid/expressions/negDivisorMod.wacc"
  , "valid/expressions/negBothDiv.wacc"
  , "valid/expressions/orExpr.wacc"
  , "valid/expressions/sequentialCount.wacc"
  , "valid/expressions/notequalsExpr.wacc"
  , "valid/expressions/negExpr.wacc"
  , "valid/expressions/andOverOrExpr.wacc"
  , "valid/expressions/stringEqualsExpr.wacc"
  , "valid/expressions/notExpr.wacc"
  , "valid/expressions/greaterExpr.wacc"
  , "valid/expressions/plusExpr.wacc"
  , "valid/expressions/charComparisonExpr.wacc"
  , "valid/expressions/modExpr.wacc"
  , "valid/expressions/minusPlusExpr.wacc"
  , "valid/expressions/minusMinusExpr.wacc"
  , "valid/expressions/boolCalc.wacc"
  , "valid/expressions/minusNoWhitespaceExpr.wacc"
  , "valid/sequence/stringAssignment.wacc"
  , "valid/sequence/charAssignment.wacc"
  , "valid/sequence/intLeadingZeros.wacc"
  , "valid/sequence/boolAssignment.wacc"
  , "valid/sequence/intAssignment.wacc"
  , "valid/sequence/basicSeq.wacc"
  , "valid/sequence/basicSeq2.wacc"
  , "valid/sequence/exitSimple.wacc"
  , "valid/while/whileBasic.wacc"
  , "valid/while/loopIntCondition.wacc"
  , "valid/while/rmStyleAddIO.wacc"
  , "valid/while/whileFalse.wacc"
  , "valid/while/fibonacciFullIt.wacc"
  , "valid/while/whileCount.wacc"
  , "valid/while/min.wacc"
  , "valid/while/max.wacc"
  , "valid/while/whileBoolFlip.wacc"
  , "valid/while/fibonacciIterative.wacc"
  , "valid/while/loopCharCondition.wacc"
  , "valid/while/rmStyleAdd.wacc"
  ]

semanticErrTests :: [FilePath]
semanticErrTests =
  [ "invalid/semanticErr/multiple/funcMess.wacc"
  , "invalid/semanticErr/multiple/ifAndWhileErrs.wacc"
  , "invalid/semanticErr/multiple/obfuscatingReturnsWithWhile.wacc"
  , "invalid/semanticErr/multiple/multiTypeErrs.wacc"
  , "invalid/semanticErr/multiple/messyExpr.wacc"
  , "invalid/semanticErr/multiple/multiCaseSensitivity.wacc"
  , "invalid/semanticErr/array/arrayIndexNotInt.wacc"
  , "invalid/semanticErr/array/noStringIndex.wacc"
  , "invalid/semanticErr/array/noArrayCovariance.wacc"
  , "invalid/semanticErr/array/nonMatchingArrays.wacc"
  , "invalid/semanticErr/array/badIndex.wacc"
  , "invalid/semanticErr/array/indexUndefIdent.wacc"
  , "invalid/semanticErr/array/arrayIndexComplexNotInt.wacc"
  , "invalid/semanticErr/array/mixingTypesInArrays.wacc"
  , "invalid/semanticErr/array/wrongArrayType.wacc"
  , "invalid/semanticErr/array/arrayMultipleIndexError.wacc"
  , "invalid/semanticErr/array/wrongArrayDimension.wacc"
  , "invalid/semanticErr/pairs/mismatchedPair.wacc"
  , "invalid/semanticErr/pairs/noPairCovariance.wacc"
  , "invalid/semanticErr/pairs/nonMatchingPairs.wacc"
  , "invalid/semanticErr/pairs/freeNonPair.wacc"
  , "invalid/semanticErr/pairs/wrongTypeInParameterlessPair.wacc"
  , "invalid/semanticErr/pairs/readUnknown.wacc"
  , "invalid/semanticErr/pairs/badPairAssign.wacc"
  , "invalid/semanticErr/pairs/badPairExchange.wacc"
  , "invalid/semanticErr/read/readIntoBadSnd.wacc"
  , "invalid/semanticErr/read/readIntoBadFst.wacc"
  , "invalid/semanticErr/read/readTypeErr01.wacc"
  , "invalid/semanticErr/if/ifIntCondition.wacc"
  , "invalid/semanticErr/IO/readTypeErr.wacc"
  , "invalid/semanticErr/scope/badScopeRedefine.wacc"
  , "invalid/semanticErr/scope/badParentScope.wacc"
  , "invalid/semanticErr/function/callUndefFunction.wacc"
  , "invalid/semanticErr/function/functionOverArgs.wacc"
  , "invalid/semanticErr/function/funcVarAccess.wacc"
  , "invalid/semanticErr/function/functionAssign.wacc"
  , "invalid/semanticErr/function/functionBadArgUse.wacc"
  , "invalid/semanticErr/function/functionBadReturn.wacc"
  , "invalid/semanticErr/function/invalidReturnsBranched.wacc"
  , "invalid/semanticErr/function/functionUnderArgs.wacc"
  , "invalid/semanticErr/function/functionRedefine.wacc"
  , "invalid/semanticErr/function/mismatchingReturns.wacc"
  , "invalid/semanticErr/function/doubleArgDef.wacc"
  , "invalid/semanticErr/function/functionBadCall.wacc"
  , "invalid/semanticErr/function/functionBadParam.wacc"
  , "invalid/semanticErr/function/functionSwapArgs.wacc"
  , "invalid/semanticErr/print/printTypeErr01.wacc"
  , "invalid/semanticErr/variables/basicTypeErr12.wacc"
  , "invalid/semanticErr/variables/basicTypeErr04.wacc"
  , "invalid/semanticErr/variables/doubleDeclare.wacc"
  , "invalid/semanticErr/variables/basicTypeErr08.wacc"
  , "invalid/semanticErr/variables/basicTypeErr09.wacc"
  , "invalid/semanticErr/variables/basicTypeErr05.wacc"
  , "invalid/semanticErr/variables/basicTypeErr02.wacc"
  , "invalid/semanticErr/variables/basicTypeErr03.wacc"
  , "invalid/semanticErr/variables/undeclaredScopeVar.wacc"
  , "invalid/semanticErr/variables/undeclaredVar.wacc"
  , "invalid/semanticErr/variables/undeclaredVarAccess.wacc"
  , "invalid/semanticErr/variables/basicTypeErr01.wacc"
  , "invalid/semanticErr/variables/basicTypeErr06.wacc"
  , "invalid/semanticErr/variables/basicTypeErr10.wacc"
  , "invalid/semanticErr/variables/caseMatters.wacc"
  , "invalid/semanticErr/variables/basicTypeErr11.wacc"
  , "invalid/semanticErr/variables/basicTypeErr07.wacc"
  , "invalid/semanticErr/expressions/mixedOpTypeErr.wacc"
  , "invalid/semanticErr/expressions/boolOpTypeErr.wacc"
  , "invalid/semanticErr/expressions/intOpTypeErr.wacc"
  , "invalid/semanticErr/expressions/exprTypeErr.wacc"
  , "invalid/semanticErr/expressions/lessPairExpr.wacc"
  , "invalid/semanticErr/expressions/moreArrExpr.wacc"
  , "invalid/semanticErr/expressions/stringElemErr.wacc"
  , "invalid/semanticErr/exit/badCharExit.wacc"
  , "invalid/semanticErr/exit/exitNonInt.wacc"
  , "invalid/semanticErr/exit/globalReturn.wacc"
  , "invalid/semanticErr/exit/returnsInMain.wacc"
  , "invalid/semanticErr/while/truErr.wacc"
  , "invalid/semanticErr/while/falsErr.wacc"
  , "invalid/semanticErr/while/whileIntCondition.wacc"
  ]

syntaxErrTests :: [FilePath]
syntaxErrTests =
  [ "invalid/syntaxErr/array/arrayExpr.wacc"
  , "invalid/syntaxErr/pairs/sndNull.wacc"
  , "invalid/syntaxErr/pairs/noNesting.wacc"
  , "invalid/syntaxErr/pairs/badLookup01.wacc"
  , "invalid/syntaxErr/pairs/badLookup02.wacc"
  , "invalid/syntaxErr/pairs/fstNull.wacc"
  , "invalid/syntaxErr/pairs/elemOfNonPair.wacc"
  , "invalid/syntaxErr/if/ifNoelse.wacc"
  , "invalid/syntaxErr/if/ifNofi.wacc"
  , "invalid/syntaxErr/if/ifNothen.wacc"
  , "invalid/syntaxErr/if/ifiErr.wacc"
  , "invalid/syntaxErr/basic/bgnErr.wacc"
  , "invalid/syntaxErr/basic/badEscape.wacc"
  , "invalid/syntaxErr/basic/beginNoend.wacc"
  , "invalid/syntaxErr/basic/noBody.wacc"
  , "invalid/syntaxErr/basic/badComment.wacc"
  , "invalid/syntaxErr/basic/badComment2.wacc"
  , "invalid/syntaxErr/basic/multipleBegins.wacc"
  , "invalid/syntaxErr/basic/skpErr.wacc"
  , "invalid/syntaxErr/basic/unescapedChar.wacc"
  , "invalid/syntaxErr/function/badlyNamed.wacc"
  , "invalid/syntaxErr/function/mutualRecursionNoReturn.wacc"
  , "invalid/syntaxErr/function/functionConditionalNoReturn.wacc"
  , "invalid/syntaxErr/function/functionLateDefine.wacc"
  , "invalid/syntaxErr/function/functionMissingPType.wacc"
  , "invalid/syntaxErr/function/functionReturnInLoop.wacc"
  , "invalid/syntaxErr/function/functionMissingCall.wacc"
  , "invalid/syntaxErr/function/functionNoReturn.wacc"
  , "invalid/syntaxErr/function/functionScopeDef.wacc"
  , "invalid/syntaxErr/function/noBodyAfterFuncs.wacc"
  , "invalid/syntaxErr/function/funcExpr2.wacc"
  , "invalid/syntaxErr/function/funcExpr.wacc"
  , "invalid/syntaxErr/function/badlyPlaced.wacc"
  , "invalid/syntaxErr/function/functionMissingParam.wacc"
  , "invalid/syntaxErr/function/functionMissingType.wacc"
  , "invalid/syntaxErr/function/functionEndingNotReturn.wacc"
  , "invalid/syntaxErr/function/thisIsNotC.wacc"
  , "invalid/syntaxErr/print/printlnCharArry.wacc"
  , "invalid/syntaxErr/variables/badintAssignments1.wacc"
  , "invalid/syntaxErr/variables/varNoName.wacc"
  , "invalid/syntaxErr/variables/badintAssignments.wacc"
  , "invalid/syntaxErr/variables/bigIntAssignment.wacc"
  , "invalid/syntaxErr/variables/badintAssignments2.wacc"
  , "invalid/syntaxErr/expressions/missingOperand2.wacc"
  , "invalid/syntaxErr/expressions/printlnConcat.wacc"
  , "invalid/syntaxErr/expressions/missingOperand1.wacc"
  , "invalid/syntaxErr/literals/stringLiteralNoNewlines.wacc"
  , "invalid/syntaxErr/literals/stringLiteralOnlyAscii.wacc"
  , "invalid/syntaxErr/literals/charLiteralSingle.wacc"
  , "invalid/syntaxErr/sequence/missingSeq.wacc"
  , "invalid/syntaxErr/sequence/emptySeq.wacc"
  , "invalid/syntaxErr/sequence/endSeq.wacc"
  , "invalid/syntaxErr/sequence/extraSeq.wacc"
  , "invalid/syntaxErr/sequence/doubleSeq.wacc"
  , "invalid/syntaxErr/while/whileNodo.wacc"
  , "invalid/syntaxErr/while/whilErr.wacc"
  , "invalid/syntaxErr/while/dooErr.wacc"
  , "invalid/syntaxErr/while/donoErr.wacc"
  , "invalid/syntaxErr/while/whileNodone.wacc"
  ]

allTests :: [FilePath]
allTests = validTests ++ semanticErrTests ++ syntaxErrTests
