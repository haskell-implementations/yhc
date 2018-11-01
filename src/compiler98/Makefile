include ../../Makefile.inc

SRCS = \
	Main.hs		Tree234.hs	IdKind.hs	Reduce.hs	\
	MergeSort.hs	AssocTree.hs	LexStr.hs	LexLow.hs	\
	NT.hs		Info.hs		Nice.hs		IntState.hs	\
	Id.hs		PrettyLib.hs	SyntaxPos.hs	ParseLex.hs	\
	ParseLib.hs	Parse2.hs	MkSyntax.hs	Syntax.hs	\
	TokenId.hs	LexPre.hs	Lex.hs		OsOnly.hs	\
	SysDeps.hs	HbcOnly.hs	PrettySyntax.hs	ParseCore.hs	\
	Parse.hs	Lexical.hs	Error.hs	Flags.hs	\
	Unlit.hs	Extra.hs	SyntaxUtil.hs	Memo.hs		\
	ImportState.hs	PreImp.hs	State.hs	IExtract.hs	\
	PreImport.hs	Overlap.hs	TokenInt.hs	NeedLib.hs	\
	Need.hs		Scc.hs		RenameLib.hs	Fixity.hs	\
	Bind.hs		Rename.hs	ParseI.hs	Import.hs	\
	Machine.hs	Prim.hs		ForeignCode.hs	PosCode.hs	\
	PosAtom.hs	STGArity.hs	PrimCode.hs	CaseOpt.hs	\
	CaseLib.hs	CaseHelp.hs	FSLib.hs	Case.hs		\
	Lift.hs		FreeVar.hs	FixSyntax.hs	Export.hs	\
	TypeData.hs	TypeSubst.hs	TypeUtil.hs	TypeEnv.hs	\
	TypeUnify.hs	TypeLib.hs	TypeCtx.hs	Extract.hs	\
	Remove1_3.hs	Type.hs		SccModule.hs	RmClasses.hs	\
	DeriveLib.hs	DeriveBinary.hs	DeriveBounded.hs		\
	DeriveIx.hs	DeriveEnum.hs	DeriveRead.hs	DeriveShow.hs	\
	DeriveOrd.hs	DeriveEq.hs	Derive.hs	FFITrans.hs	\
	StrPos.hs	ReportImports.hs		Set.hs		\
	BCWrap.hs	BCPeep.hs	BCRelative.hs	BCGraph.hs	\
	BCFlatten.hs	BCAnalysis.hs	BCShow.hs	BCCompile2.hs 	\
	BCode.hs	BCWrite.hs	Depend.hs	StateMonad.hs

OBJDIR = obj
DST    = ../../inst/bin
TARGET = $(DST)/yhc

HMAKE  = hmake
HMAKEFLAGS = -package base

.PHONY: all clean compiler

all: compiler

compiler:
# $(TARGET): $(OBJDIR) $(SRCS)
	$(HC) --make Main -i.:../../depends/cpphs -o $(TARGET)


#	$(HMAKE) -hc=$(HC) $(HMAKEFLAGS) -d $(OBJDIR) Main
#	mv $(OBJDIR)/Main$(EXE) $(TARGET)

$(OBJDIR):
	mkdir -p $(OBJDIR)

clean:
	rm -f $(TARGET) $(SRCS:%.hs=$(OBJDIR)/%.o) $(SRCS:%.hs=%.hi)
