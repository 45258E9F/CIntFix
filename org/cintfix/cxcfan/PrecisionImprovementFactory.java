package org.cintfix.cxcfan;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;

import org.eclipse.cdt.core.dom.ast.IASTASMDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTArrayDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTArraySubscriptExpression;
import org.eclipse.cdt.core.dom.ast.IASTBinaryExpression;
import org.eclipse.cdt.core.dom.ast.IASTBreakStatement;
import org.eclipse.cdt.core.dom.ast.IASTCaseStatement;
import org.eclipse.cdt.core.dom.ast.IASTCastExpression;
import org.eclipse.cdt.core.dom.ast.IASTCompositeTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTCompoundStatement;
import org.eclipse.cdt.core.dom.ast.IASTConditionalExpression;
import org.eclipse.cdt.core.dom.ast.IASTContinueStatement;
import org.eclipse.cdt.core.dom.ast.IASTDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTDeclarationStatement;
import org.eclipse.cdt.core.dom.ast.IASTDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTDefaultStatement;
import org.eclipse.cdt.core.dom.ast.IASTDoStatement;
import org.eclipse.cdt.core.dom.ast.IASTElaboratedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEnumerationSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTEqualsInitializer;
import org.eclipse.cdt.core.dom.ast.IASTExpression;
import org.eclipse.cdt.core.dom.ast.IASTExpressionStatement;
import org.eclipse.cdt.core.dom.ast.IASTFieldDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTFieldReference;
import org.eclipse.cdt.core.dom.ast.IASTForStatement;
import org.eclipse.cdt.core.dom.ast.IASTFunctionCallExpression;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTFunctionDefinition;
import org.eclipse.cdt.core.dom.ast.IASTGotoStatement;
import org.eclipse.cdt.core.dom.ast.IASTIdExpression;
import org.eclipse.cdt.core.dom.ast.IASTIfStatement;
import org.eclipse.cdt.core.dom.ast.IASTInitializer;
import org.eclipse.cdt.core.dom.ast.IASTInitializerClause;
import org.eclipse.cdt.core.dom.ast.IASTLabelStatement;
import org.eclipse.cdt.core.dom.ast.IASTLiteralExpression;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNamedTypeSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.eclipse.cdt.core.dom.ast.IASTNullStatement;
import org.eclipse.cdt.core.dom.ast.IASTPointerOperator;
import org.eclipse.cdt.core.dom.ast.IASTPreprocessorIncludeStatement;
import org.eclipse.cdt.core.dom.ast.IASTReturnStatement;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclSpecifier;
import org.eclipse.cdt.core.dom.ast.IASTSimpleDeclaration;
import org.eclipse.cdt.core.dom.ast.IASTStandardFunctionDeclarator;
import org.eclipse.cdt.core.dom.ast.IASTStatement;
import org.eclipse.cdt.core.dom.ast.IASTSwitchStatement;
import org.eclipse.cdt.core.dom.ast.IASTTranslationUnit;
import org.eclipse.cdt.core.dom.ast.IASTTypeId;
import org.eclipse.cdt.core.dom.ast.IASTUnaryExpression;
import org.eclipse.cdt.core.dom.ast.IASTWhileStatement;
import org.eclipse.cdt.core.dom.ast.IBasicType;
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind;
import org.eclipse.cdt.core.dom.ast.IEnumeration;
import org.eclipse.cdt.core.dom.ast.IFunctionType;
import org.eclipse.cdt.core.dom.ast.IPointerType;
import org.eclipse.cdt.core.dom.ast.IQualifierType;
import org.eclipse.cdt.core.dom.ast.IType;
import org.eclipse.cdt.core.dom.ast.ITypedef;
import org.eclipse.cdt.core.dom.ast.gnu.c.GCCLanguage;
import org.eclipse.cdt.core.parser.DefaultLogService;
import org.eclipse.cdt.core.parser.FileContent;
import org.eclipse.cdt.core.parser.IParserLogService;
import org.eclipse.cdt.core.parser.IScannerInfo;
import org.eclipse.cdt.core.parser.IncludeFileContentProvider;
import org.eclipse.cdt.core.parser.OffsetLimitReachedException;
import org.eclipse.cdt.core.parser.ScannerInfo;
import org.eclipse.cdt.internal.core.dom.parser.c.CBasicType;
import org.eclipse.cdt.internal.core.dom.parser.c.CPointerType;
import org.eclipse.cdt.internal.core.parser.scanner.ILexerLog;
import org.eclipse.cdt.internal.core.parser.scanner.Lexer;
import org.eclipse.cdt.internal.core.parser.scanner.Lexer.LexerOptions;
import org.eclipse.cdt.internal.core.parser.scanner.Token;
import org.eclipse.core.runtime.CoreException;
import org.sosy_lab.common.Pair;

import com.google.common.collect.Lists;

import scala.actors.threadpool.Arrays;


public class PrecisionImprovementFactory {
	
	private String inputCFile;
	private String inputIFile; // this is the corresponding *.i file which is pre-processed
	
	private static String prefix = "__HIGHPREC_";
	private static String tempPrefix = "__HIGHPREC_INTERM_";
	private static int tempPfLen = prefix.length();
	
	private static String mpz_t = "mpz_t"; // GMP BigInteger type
	private static String mpz_init = "mpz_init(%s); ";
	private static String mpz_clear = "mpz_clear(%s); ";
	private static String mpz_set_ui = "mpz_set_ui(%s, %s)";
	private static String mpz_set_si = "mpz_set_si(%s, %s)";
	private static String mpz_set_str = "mpz_set_str(%s, %s, %s)";
	private static String mpz_set = "mpz_set(%s, %s)";
	private static String mpz_cmp = "mpz_cmp(%s, %s)";
	private static String mpz_cmp_si = "mpz_cmp_si(%s, %s)";
	private static String mpz_add = "mpz_add(%s, %s, %s)";
	private static String mpz_sub = "mpz_sub(%s, %s, %s)";
	private static String mpz_mul = "mpz_mul(%s, %s, %s)";
	private static String mpz_tdiv_q = "mpz_tdiv_q(%s, %s, %s)"; // div
	private static String mpz_tdiv_r = "mpz_tdiv_r(%s, %s, %s)"; // mod
	// bit-shift operation should be with great care! Negative shift is prohibited.
	// the third parameter is of type mp_bitcnt_t, which is equivalent to unsigned long now.
	private static String mpz_mul_2exp = "mpz_mul_2exp(%s, %s, %s)"; // left-shift
	private static String mpz_fdiv_q_2exp = "mpz_fdiv_q_2exp(%s, %s, %s)"; // right-shift
	private static String mpz_neg = "mpz_neg(%s, %s)";
	private static String mpz_add_ui = "mpz_add_ui(%s, %s, %s)";
	private static String mpz_sub_ui = "mpz_sub_ui(%s, %s, %s)";
	
	// __CHECK_GMP_UINT(mpz_t);
	private static String check_gmp_uint = "__CHECK_GMP_UINT(%s)"; // use mpz_fits_* function first, then use mpz_get_* function to restore its value
	private static String check_gmp_int = "__CHECK_GMP_INT(%s)";
	private static String check_gmp_slong = "__CHECK_GMP_SLONG(%s)";
	private static String check_gmp_ulong = "__CHECK_GMP_ULONG(%s)";
	private static String check_gmp_ushort = "__CHECK_GMP_USHORT(%s)";
	private static String check_gmp_sshort = "__CHECK_GMP_SSHORT(%s)";
	private static String check_gmp_uchar = "__CHECK_GMP_UCHAR(%s)";
	private static String check_gmp_schar = "__CHECK_GMP_SCHAR(%s)";
	
	// __CHECK_SIMPLE_UINT(unsigned long int, int);
	// 2 parameters: 1) input integer, 2) signed (1) or unsigned (0)
	private static String check_uint = "__CHECK_SIMPLE_UINT(%s, %s)";
	private static String check_int = "__CHECK_SIMPLE_INT(%s, %s)";
	private static String check_slong = "__CHECK_SIMPLE_SLONG(%s, %s)";
	private static String check_ulong = "__CHECK_SIMPLE_ULONG(%s, %s)";
	private static String check_sshort = "__CHECK_SIMPLE_SSHORT(%s, %s)";
	private static String check_ushort = "__CHECK_SIMPLE_USHORT(%s, %s)";
	private static String check_schar = "__CHECK_SIMPLE_SCHAR(%s, %s)";
	private static String check_uchar = "__CHECK_SIMPLE_UCHAR(%s, %s)";
	
	// __CHECK_POINTER_PLUS(unsigned long, unsigned long / signed long)
	// __CHECK_POINTER_MINUS(unsigned long, unsigned long / signed long)
	// the program halts if the result address exceeds the address range (0~2^64-1)
	// this function has no return value!
	private static String check_pointer_plus_ul = "__CHECK_POINTER_PLUS_UL(%s, %s)";
	private static String check_pointer_plus_sl = "__CHECK_POINTER_PLUS_SL(%s, %s)";
	private static String check_pointer_minus_ul = "__CHECK_POINTER_MINUS_UL(%s, %s)";
	private static String check_pointer_minus_sl = "__CHECK_POINTER_MINUS_SL(%s, %s)";
	
	// __CALC_BRANCH_HASH (int x, ...) ---> this function have variable argument list
	// return: an integer (int) corresponds to the number of branch
	private static String calc_branch = "__CALC_BRANCH_HASH(%s)";
	
	// a template for temporary variable for return expression
	private static String ret_prefix = "__ret_value_%s";
	
	private static String legalSpecifier = "diuoxXfFeEgGaAcspn%";
	
	private static String[] extraIncludes = { "#include <gmp.h>", "#include \"intfixtoolkit.h\"" };
	
	private Set<String> localVariables; // since we should ensure that a variable cannot be disposed twice	
	private Stack<Set<String>> localVariableStack;
	// tempVarInFunc records temporary variables that have been used in current function
	// tempVarTable records temporary variable availability table, which is used to optimize temporary variable distribution
	private Set<String> tempVarInScope; // functions akin to localVariables
	private Set<String> tempVarFrozen;
	private Map<String, Boolean> tempVarTable;
	private int tempVarNum = 10; // Number of intermediate variables required
	private int tempVarDelta = 5; // Extend the table once for a chunk
	private boolean nobrace = false;
	
	private Map<String, String> pointerInfo; // this mapping structure stores pointer information on GMP variables (and their ordinary versions)
	private Map<String, Boolean> updateInfo; // this mapping records to-be-updated GMP variables and signs of corresponding ordinary variables. For handling function calls only.
	private Map<String, Boolean> signTable; // this is scope-wise
	private boolean isFuncCall = false;
	
	private IASTFunctionDefinition visiting = null;
	
	public PrecisionImprovementFactory(String fileName) {
		this.inputCFile = fileName;
		this.inputIFile = fileName.substring(0, fileName.length() - 1).concat("i");
		
		this.localVariables = new HashSet<String>();
		
		// initialize temporary variable table
		tempVarTable = new HashMap<String, Boolean>();
		for(int i = 1; i <= tempVarNum; i++) {
			String varName = tempPrefix + String.valueOf(i);
			tempVarTable.put(varName, true);
		}
		
		tempVarInScope = new HashSet<>();
		tempVarFrozen = new HashSet<>();
		
		/*
		 * (1) information in stack is relevant to only one function at one time;
		 * (2) different elements in stack record stratification info;
		 */
		localVariableStack = new Stack<>();
		
		/*
		 * we have some assumptions:
		 * (1) new pointer relation can be introduced only by assignment;
		 * (2) why we need to maintain pointer relations? Because some statements may modify value of variable by operating pointer!
		 *     We assume that only assignment and function call can operate pointer to modify variable
		 */
		pointerInfo = new HashMap<>();
		
		updateInfo = new HashMap<>();
		
		signTable = new HashMap<>();
	}
	
	public void perform() throws Exception {
		
		// FIRST, parse input C file into AST for further analysis
		File CFile = new File(inputCFile);
		if(!CFile.exists()) {
			System.err.println("Input program " + inputCFile + " is not a valid file!");
			System.exit(1);
		}
		File IFile = new File(inputIFile);
		if(!IFile.exists()) {
			System.err.println("Input preprocessed program " + inputIFile + " is not a valid file!");
			System.exit(1);
		}
		
		// before analyzing C program, we need to preprocess it first to remove some syntactic elements such as macros
		CPreprocessor preprocessor = new CPreprocessor(inputCFile, inputIFile);
		String refinedCode = preprocessor.processCode();
		String[] includeStmts = preprocessor.getIncludeStatements();
		
		FileContent content = FileContent.create(inputCFile, refinedCode.toCharArray());
		IScannerInfo info = new ScannerInfo();
		IParserLogService log = new DefaultLogService();
		IncludeFileContentProvider emptyIncludes = IncludeFileContentProvider.getEmptyFilesProvider();
		IASTTranslationUnit transUnit = GCCLanguage.getDefault().getASTTranslationUnit(content, info, emptyIncludes, null, 8, log);
		
		// SECOND, extract elements for conversion
		/*
		 * How to create a highly-precise version of code?
		 * (1) Extract top level AST elements, including function definition, global variable declaration.
		 *     Function declaration is not included because we cannot alter the signature.
		 * (2) Create highly-precise code in the function-wise manner.
		 */
		
		IASTNode[] topNodeSet = transUnit.getChildren();
		List<String> outputCode = new ArrayList<String>();
		
		// in general, global declarations are at the top of program
		// in order to correctly recognize all integer operations, we should maintain type alias information
		Map<String, String> typeAlias = new HashMap<String, String>();
		
		// how to recognize the variable to be lifted up to GMP integer? By storing them in some structures
		// we can store these variables into a map structure, whose entry consists of (1) variable name; (2) initial value in literal form
		Set<String> liftedVars = new HashSet<String>();
		boolean dontSkip = false;
		for(IASTNode topNode : topNodeSet) {
			// ignore nodes which are not belong to current C file.
			int startLn = topNode.getFileLocation().getStartingLineNumber();
			// Theoretically, one AST node is belong to one file.
			String containFile = preprocessor.getContainingFile(startLn);
			
			dontSkip = (containFile.equals(inputCFile) || inputCFile.endsWith(containFile));
			
			if(topNode instanceof IASTFunctionDefinition) {
				if(!dontSkip) {
					continue;
				}
				String convertedCode = convertToGMP(topNode, typeAlias, liftedVars);
				outputCode.add(convertedCode);
			} else if(topNode instanceof IASTSimpleDeclaration) {
				// we decide to keep global variable declarations for several reasons:
				// (1) it is a bad program design to use many global variables and modify them in multiple functions
				// (2) it is infeasible to simply replace global integer declaration with GMP version. We can initialize them in the top of main function, but we can
				//     get to know all global variables until we scan all C files
				if(isGlobalVariableDeclaration((IASTSimpleDeclaration)topNode)) {
					String convertedCode = convertToGMP(topNode, typeAlias, liftedVars);
					if(!dontSkip) {
						continue;
					} else {
						outputCode.add(convertedCode);
					}
				} else {
					// this corresponds to function declaration
					if(!dontSkip) {
						continue;
					} else {
						outputCode.add(topNode.getRawSignature());
					}
				}
			} else {
				if(!dontSkip) {
					continue;
				} else {
					outputCode.add(topNode.getRawSignature());
				}
			}
		}
		
		// Write back GMP version of C program
		String newFileName = genNewFile(inputCFile);
		BufferedWriter bout = new BufferedWriter(new FileWriter(newFileName));
		for(String includeStmt : includeStmts) {
			bout.write(includeStmt);
			bout.newLine();
		}
		bout.flush();
		
		// add two include files if necessary: (1) gmp.h; (2) intfixtoolkit.h
		for(String extraInclude : extraIncludes) {
			if(!preprocessor.includeFileExists(extraInclude)) {
				bout.write(extraInclude);
				bout.newLine();
			}
		}
		bout.flush();
		
		for(int i = 0; i < outputCode.size(); i++) {
			String line = outputCode.get(i);
			bout.write(line);
			bout.newLine();
		}
		bout.flush();
		bout.close();
	}
	
	private String genNewFile(String oldFileName) {
		int dotIndex = oldFileName.lastIndexOf('.');
		String extName = oldFileName.substring(dotIndex);
		String remain = oldFileName.substring(0, dotIndex);
		return remain + ".hp" + extName;
	}
	
	private String convertToGMP(IASTNode topNode, Map<String, String> typeAlias, Set<String> liftedVars) {
		if(topNode instanceof IASTSimpleDeclaration) {
			return handleSimpleDeclaration((IASTSimpleDeclaration)topNode, typeAlias, liftedVars, true);
		} else {
			return handleFunctionDefinition((IASTFunctionDefinition)topNode, typeAlias, liftedVars);
		}
	}
	
	private String toIntLiteral(String intLit) {
		int length = intLit.length();
		int cutPos = length - 1;
		for(cutPos = length - 1; cutPos >= 0; cutPos--) {
			char thisChar = intLit.charAt(cutPos);
			if(thisChar == 'l' || thisChar == 'u' || thisChar == 'L' || thisChar == 'U') {
				continue;
			} else {
				break;
			}
		}
		if(cutPos != (length - 1)) {
			return intLit.substring(0, cutPos + 1);
		} else {
			return intLit;
		}
	}
	
	private String handleFunctionDefinition(IASTFunctionDefinition funcNode, Map<String, String> typeAlias, Set<String> liftedVars) {
		/*
		 * Function definition has three key components:
		 * (1) specifier, which refers to type;
		 * (2) declarator, which refers to the function name and its parameter list;
		 * (3) body, which is a statement. Typically, a complex definition has a COMPOUND body composed with many statements.
		 * 
		 */
		
		// intermediate variables can be created and used inside function definition. An instance cannot be accessed by multiple functions in the program!
		// That means, we have to maintain variable creation and destruction in every function!! 
		
		String newFuncDef = "";
		int indent = 0; // this is used for indent control
		
		// STEP 1: copy the global alias list for analyzing function body since some of them could be overwritten!!
		// **NOTE: List of type alias consists of global info, but list of lifted variables does not.
		// type alias and lifted variables are scope-relevant
		
		// STEP 2: generate the signature of this function, which is consistent with the original
		IASTDeclSpecifier funcSpecifier = funcNode.getDeclSpecifier();
		IASTFunctionDeclarator funcDeclarator = funcNode.getDeclarator();
		IASTStatement funcBody = funcNode.getBody();
		String funcSig = funcSpecifier.getRawSignature() + " " + funcDeclarator.getRawSignature();
		
		// to support type check in return statement, we store function declaration info in global
		// for every call of this function, the specifier should be updated
		visiting = funcNode;
		
		// Although unnecessary, we set 'nobrace' to its default value
		nobrace = false;
		
		// We assume that function definitions can only be top nodes of current translation unit.
		localVariables.clear();
		localVariableStack.clear();
		tempVarInScope.clear();
		tempVarFrozen.clear();
		reinitializeVarMap(tempVarTable);
		
		// STEP 3: analyze function body
		String funcBodyCode = handleFunctionBody(funcBody, typeAlias, liftedVars, indent);
	
		// after analyzing this function, reset visiting to null
		visiting = null;
		
		newFuncDef = newFuncDef.concat(funcSig).concat("\n").concat(funcBodyCode);
		return newFuncDef;
	}
	
	/*
	 *  a function with indent parameter should output text with appropriate indent
	 *  no function is responsible for generating a newline at the end of the text
	 *  *** however, you are responsible to maintain indent inside code block
	 */
	private String handleFunctionBody(IASTStatement funcBody, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		// GCC does not accept the function definition where body is not enclosed with curly braces
		// Hence, for now, we assume that the statement of function body must be of IASTCompoundStatement
		assert (funcBody instanceof IASTCompoundStatement);
		return handleCompoundStatement((IASTCompoundStatement)funcBody, typeAlias, liftedVars, indent);
	}
	
	private String handleStatement(IASTStatement statement, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		// recognize the correct type of statement, and call corresponding handling function
		String retStmt = "";
		if(statement instanceof IASTDeclarationStatement) {
			retStmt = handleDeclarationStatement((IASTDeclarationStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTExpressionStatement) {
			retStmt = handleExpressionStatement((IASTExpressionStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTCompoundStatement) {
			retStmt = handleCompoundStatement((IASTCompoundStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTIfStatement) {
			retStmt = handleIfStatement((IASTIfStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTForStatement) {
			retStmt = handleForStatement((IASTForStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTWhileStatement) {
			retStmt = handleWhileStatement((IASTWhileStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTBreakStatement) {
			retStmt = handleBreakStatement((IASTBreakStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTContinueStatement) {
			retStmt = handleContinueStatement((IASTContinueStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTDoStatement) {
			retStmt = handleDoStatement((IASTDoStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTSwitchStatement) {
			retStmt = handleSwitchStatement((IASTSwitchStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTReturnStatement) {
			retStmt = handleReturnStatement((IASTReturnStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTLabelStatement) {
			retStmt = handleLabelStatement((IASTLabelStatement)statement, typeAlias, liftedVars, indent);			
		} else if(statement instanceof IASTGotoStatement) {
			retStmt = handleGotoStatement((IASTGotoStatement)statement, typeAlias, liftedVars, indent);
		} else if(statement instanceof IASTNullStatement) {
			retStmt = handleNullStatement((IASTNullStatement)statement, typeAlias, liftedVars, indent);
		} else {
			// unexpected case
			retStmt = statement.getRawSignature();
		}
		
		// After function call, we should update GMP variables
		if(isFuncCall == true) {
			isFuncCall = false;
			StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
			String indentStr = String.format(indentBuilder.toString(), "");
			retStmt = retStmt.concat("\n");
			for(Entry<String, Boolean> entry : updateInfo.entrySet()) {
				String newName = entry.getKey();
				boolean sign = entry.getValue();
				String origName = newName.substring(tempPfLen);
				if(sign) {
					retStmt = retStmt.concat(indentStr).concat(String.format(mpz_set_si, newName, origName)).concat(";\n");
				} else {
					retStmt = retStmt.concat(indentStr).concat(String.format(mpz_set_ui, newName, origName)).concat(";\n");
				}
			}
		}
		
		updateInfo.clear();
		return retStmt;
	}
	
	private String handleNullStatement(IASTNullStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		// (;)
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2 + 1).append("s");
		return String.format(indentBuilder.toString(), ";");
	}
	
	private String handleGotoStatement(IASTGotoStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		// keep this statement
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		String newCode = String.format(indentBuilder.toString(), "").concat(stmt.getRawSignature()); // raw signature is complete, with semicolon at tail
		return newCode;
	}
	
	private String handleLabelStatement(IASTLabelStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		/*
		 * A label statement consists of a label and a nested statement.
		 * Nested statement is required, if we don't want to do anything after this label, we can simply write a null statement (;) before this label
		 */
		String newCode = "";
		String labelName = stmt.getName().getRawSignature();
		IASTStatement nestStmt = stmt.getNestedStatement();
		
		// in general, there is no indent before the label
		newCode = newCode.concat(labelName).concat(":\n");
		newCode = newCode.concat(handleStatement(nestStmt, typeAlias, liftedVars, indent));
		
		return newCode;
	}
	
	private String handleReturnStatement(IASTReturnStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		/*
		 * Before return from this function, we should carefully check the return expression to see if its value overflows
		 * 
		 * by executing return statement, we will exit from this function, thus all GMP variables should be destroyed manually
		 *        (1) temporary variables of lists in stack are disjoint;
		 *        (2) it is possible that we cannot clear the memory of all local variables completely because of duplicated variable names. 
		 */
		assert (visiting != null) : "Return statement must be in a function definition!";
		String newCode = "";
		IASTExpression returnValue = stmt.getReturnValue();
		
		// check assertion is required only when the return type of this function is integer
		IASTDeclSpecifier funcSpec = visiting.getDeclSpecifier();
		IASTFunctionDeclarator funcDecl = visiting.getDeclarator();
		
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		String indentStr = String.format(indentBuilder.toString(), "");
		
		// FIRST, scan the expression if it is not null
		if(returnValue != null) {
			Pair<String, String> evalRet = scanExpressionInDFS(returnValue, typeAlias, tempVarTable, liftedVars);
			reinitializeVarMap(tempVarTable);
			newCode = newCode.concat(insertCodeWithIndent(indentStr, evalRet.getFirst()));
			String retValName = evalRet.getSecond();
			
			Pair<Integer, Boolean> specInfo = getFinalType(funcSpec, typeAlias);
			int slength = specInfo.getFirst();
			boolean ssign = specInfo.getSecond();
			
			// we need to create another temporary variable (of internal integer type) for return variable in order to
			//        completely release memory (otherwise memory leak will occur).
			String retVarName = String.format(ret_prefix, funcDecl.getName().toString());
			
			if(slength == -1) {
				// return type of this function should not be numerical
				newCode = newCode.concat(indentStr).concat("return ").concat(retValName).concat(";");
			} else {
				// return type can be numerical, which depends on declarator
				IASTPointerOperator[] asterisk = funcDecl.getPointerOperators();
				if(asterisk.length == 0) {
					// then we can confirm that the return type is purely numerical
					// SECOND, assert check to guarantee that the return value precisely matches the return type of function

					// how about the return expression is a GMP integer?
					if(isHighPrecisionVar(retValName, tempVarTable, liftedVars)) {
						if(ssign) {
							// signed
							if(slength == 1) {
								newCode = newCode.concat(indentStr).concat("char ").concat(retVarName).concat(" = ").concat(String.format(check_gmp_schar, retValName)).concat(";\n");
							} else if(slength == 2) {
								newCode = newCode.concat(indentStr).concat("short ").concat(retVarName).concat(" = ").concat(String.format(check_gmp_sshort, retValName)).concat(";\n");
							} else if(slength == 4) {
								newCode = newCode.concat(indentStr).concat("int ").concat(retVarName).concat(" = ").concat(String.format(check_gmp_int, retValName)).concat(";\n");
							} else {
								newCode = newCode.concat(indentStr).concat("long ").concat(retVarName).concat(" = ").concat(String.format(check_gmp_slong, retValName)).concat(";\n");
							}
						} else {
							// unsigned
							if(slength == 1) {
								newCode = newCode.concat(indentStr).concat("unsigned char ").concat(retVarName).concat(" = ").concat(String.format(check_gmp_uchar, retValName)).concat(";\n");
							} else if(slength == 2) {
								newCode = newCode.concat(indentStr).concat("unsigned short ").concat(retVarName).concat(" = ").concat(String.format(check_gmp_ushort, retValName)).concat(";\n");
							} else if(slength == 4) {
								newCode = newCode.concat(indentStr).concat("unsigned int ").concat(retVarName).concat(" = ").concat(String.format(check_gmp_uint, retValName)).concat(";\n");
							} else {
								newCode = newCode.concat(indentStr).concat("unsigned long ").concat(retVarName).concat(" = ").concat(String.format(check_gmp_ulong, retValName)).concat(";\n");
							}
						}
					} else {
						Pair<Integer, Boolean> retType = getFinalType(returnValue, typeAlias);
						String retSign = retType.getSecond() ? "1" : "0";
						
						if(ssign) {
							// signed
							if(slength == 1) {
								newCode = newCode.concat(indentStr).concat("char ").concat(retVarName).concat(" = ").concat(String.format(check_schar, retValName, retSign)).concat(";\n");
							} else if(slength == 2) {
								newCode = newCode.concat(indentStr).concat("short ").concat(retVarName).concat(" = ").concat(String.format(check_sshort, retValName, retSign)).concat(";\n");
							} else if(slength == 4) {
								newCode = newCode.concat(indentStr).concat("int ").concat(retVarName).concat(" = ").concat(String.format(check_int, retValName, retSign)).concat(";\n");
							} else {
								newCode = newCode.concat(indentStr).concat("long ").concat(retVarName).concat(" = ").concat(String.format(check_slong, retValName, retSign)).concat(";\n");
							}
						} else {
							// unsigned
							if(slength == 1) {
								newCode = newCode.concat(indentStr).concat("unsigned char ").concat(retVarName).concat(" = ").concat(String.format(check_uchar, retValName, retSign)).concat(";\n");
							} else if(slength == 2) {
								newCode = newCode.concat(indentStr).concat("unsigned short ").concat(retVarName).concat(" = ").concat(String.format(check_ushort, retValName, retSign)).concat(";\n");
							} else if(slength == 4) {
								newCode = newCode.concat(indentStr).concat("unsigned int ").concat(retVarName).concat(" = ").concat(String.format(check_uint, retValName, retSign)).concat(";\n");
							} else {
								newCode = newCode.concat(indentStr).concat("unsigned long ").concat(retVarName).concat(" = ").concat(String.format(check_ulong, retValName, retSign)).concat(";\n");
							}
						}
					}
				} else {
					// still not numerical
					String tempDecl = "";
					String retSpec = funcSpec.getRawSignature();
					tempDecl = tempDecl.concat(indentStr).concat(retSpec).concat(" ");
					for(int i = 0; i < asterisk.length; i++) {
						tempDecl = tempDecl.concat("*");
					}
					tempDecl = tempDecl.concat(retVarName).concat(" = ").concat(retValName).concat(";\n");
					newCode = newCode.concat(tempDecl);
				} // whether pointer
				// destroy used variables
				// ** NOTE: destroyGen is responsible to add '\n' and semicolon at the end of statement
				newCode = newCode.concat(destroyGenerate(indentStr));
				newCode = newCode.concat(indentStr).concat("return ").concat(retVarName).concat(";");
			} // whether numerical
		} else {
			// no check is required
			newCode = newCode.concat(destroyGenerate(indentStr));
			// destroy used variables
			newCode = "return;";
		}
		return newCode;		
	}
	
	private String handleSwitchStatement(IASTSwitchStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		/*
		 * switch statements are used with case/default statements. According to the standard of C, expression of switch statement could only be integers (enumerator is also integer)
		 * expression of case statement could only be constant integers (excluding mutable variables)
		 * 
		 * Therefore, here is our method to handle switch-case-default structure:
		 * (1) read the controller expression of switch statement
		 * (2) read the expression of case statements
		 * (3) compare the controller expression with case expressions to generate a new controller expression
		 * (4) rewrite case expressions accordingly
		 * 
		 * ** NOTE: statements before the first case are ignored in practice. Thus when we consider the body of switch statement, we starts from the first case statement
		 */
		IASTExpression controller = stmt.getControllerExpression();
		IASTStatement switchBody = stmt.getBody();
		String newCode = "";
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		String indentStr = String.format(indentBuilder.toString(), "");
		StringBuilder indentBuilderInner = new StringBuilder("%1$").append(indent * 2 + 2).append("s");
		String indentStrInner = String.format(indentBuilderInner.toString(), "");
		
		// we assume that the body of switch is a compound statement enclosed by curly braces
		assert (switchBody instanceof IASTCompoundStatement);
		IASTStatement[] children = ((IASTCompoundStatement)switchBody).getStatements();
		
		// case statements are expected in this set
		List<IASTExpression> caseExpressions = new ArrayList<IASTExpression>();
		for(IASTStatement child : children) {
			if(child instanceof IASTCaseStatement) {
				caseExpressions.add(((IASTCaseStatement)child).getExpression());
			}
		}
		
		List<String> caseList = new ArrayList<String>(); // this list is for constructing parameter list
		
		// OK! Remember that controller expression must be integer expression (or enumeration), and case expressions are constant integer expressions!
		Pair<String, String> evalControl = scanExpressionInDFS(controller, typeAlias, tempVarTable, liftedVars);
		// DO NOT restore the state of temporary variable table since expressions should be assigned with different temporary variables!
		newCode = newCode.concat(insertCodeWithIndent(indentStr, evalControl.getFirst()));
		String controlName = evalControl.getSecond();
		String controlValue = "";
		if(!isHighPrecisionVar(controlName, tempVarTable, liftedVars)) {
			// we need to wrap this value with a GMP integer
			Pair<Integer, Boolean> controlType = getFinalType(controller, typeAlias);
			controlValue = getVar(tempVarTable);
			if((controlType.getSecond() == false && controlType.getFirst() == 8) || controlType.getFirst() == -1) {
				newCode = newCode.concat(indentStr).concat(String.format(mpz_set_ui, controlValue, controlName)).concat(";\n");
			} else {
				newCode = newCode.concat(indentStr).concat(String.format(mpz_set_si, controlValue, controlName)).concat(";\n");
			}
		} else {
			controlValue = controlName;
		}
		// at the end of this block, controlValue must be a GMP integer!
		
		for(int i = 0; i < caseExpressions.size(); i++) {
			// case expression should be a constant expression (literal, enumerator, etc.)
			IASTExpression branch = caseExpressions.get(i);
			Pair<String, String> evalBranch = scanExpressionInDFS(branch, typeAlias, tempVarTable, liftedVars);
			newCode = newCode.concat(insertCodeWithIndent(indentStr, evalBranch.getFirst()));
			String branchName = evalBranch.getSecond();
			String branchValue = "";
			if(!isHighPrecisionVar(branchName, tempVarTable, liftedVars)) {
				// then we need to wrap it using a GMP integer
				Pair<Integer, Boolean> branchType = getFinalType(branch, typeAlias);
				branchValue = getVar(tempVarTable);
				if((branchType.getSecond() == false && branchType.getFirst() == 8) || branchType.getFirst() == -1) {
					newCode = newCode.concat(indentStr).concat(String.format(mpz_set_ui, branchValue, branchName)).concat(";\n");
				} else {
					newCode = newCode.concat(indentStr).concat(String.format(mpz_set_si, branchValue, branchName)).concat(";\n");
				}
			} else {
				branchValue = branchName;
			}
			caseList.add(branchValue);
		}
		
		// Then, we will start to construct the parameter list for function __CALC_BRANCH_HASH(...)
		List<String> parameterList = new ArrayList<String>();
		for(int i = 0; i < caseList.size(); i++) {
			String thisValue = caseList.get(i);
			String component = String.format(mpz_cmp, controlValue, thisValue) + " == 0 ? 1 : 0"; // if equals, then return 1; otherwise return 0
			parameterList.add(component);
		}
		
		// Then, refine it as a true parameter list: param1, param2, ..., paramN
		String argument = "";
		int paramLength = parameterList.size();
		argument = argument.concat(String.valueOf(paramLength)).concat(", ");
		for(int i = 0; i < paramLength - 1; i++) {
			argument = argument.concat(parameterList.get(i)).concat(", ");
		}
		if(paramLength > 0) {
			argument = argument.concat(parameterList.get(paramLength - 1));
		}
		
		// Then construct new controller expression
		newCode = newCode.concat(indentStr).concat("switch(").concat(String.format(calc_branch, argument)).concat(")\n");
		newCode = newCode.concat(indentStr).concat("{\n");
		// here we reconstruct case statements and others
		reinitializeVarMap(tempVarTable);
		int caseNumber = 0;
		for(IASTStatement child : children) {
			if(child instanceof IASTCaseStatement) {
				caseNumber++;
				newCode = newCode.concat(indentStrInner).concat("case ").concat(String.valueOf(caseNumber)).concat(":\n");
			} else if(child instanceof IASTDefaultStatement) {
				newCode = newCode.concat(indentStrInner).concat("default:\n");
			} else {
				// other statements, handle them wisely
				if(caseNumber > 0) {
					// if caseNumber == 0, then this statement locates before the first case statement. This statement should be ignored
					newCode = newCode.concat(handleStatement(child, typeAlias, liftedVars, indent + 1)).concat("\n");
				}
			}
		}
		
		newCode = newCode.concat(indentStr).concat("}");
		
		return newCode;
	}
	
	private String handleDoStatement(IASTDoStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		String newCode = "";
		IASTStatement doBody = stmt.getBody();
		IASTExpression condition = stmt.getCondition();
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		String indentStr = String.format(indentBuilder.toString(), "");
		StringBuilder indentBuilderInner = new StringBuilder("%1$").append(indent * 2 + 2).append("s");
		String indentStrInner = String.format(indentBuilderInner.toString(), "");
		
		newCode = newCode.concat(indentStr).concat("do {\n");
		
		// backup here
		Set<String> localVariablesBK = new HashSet<>();
		Set<String> tempVarInScopeBK = new HashSet<>();
		Map<String, String> typeAliasBK = new HashMap<>();
		Set<String> liftedVarsBK = new HashSet<>();
		Map<String, Boolean> signTableBK = new HashMap<>();
		copySetFromTo(localVariables, localVariablesBK);
		localVariableStack.push(localVariablesBK);
		localVariables.clear();
		tempVarFrozen.addAll(tempVarInScope);
		copySetFromTo(tempVarInScope, tempVarInScopeBK);
		tempVarInScope.clear();
		copyMapFromTo(typeAlias, typeAliasBK);
		copySetFromTo(liftedVars, liftedVarsBK);
		copyMapFromTo(signTable, signTableBK);
		// END
		
		String bodyCode = "";
		if(doBody instanceof IASTCompoundStatement) {
			nobrace = true;
			bodyCode = bodyCode.concat(handleStatement(doBody, typeAlias, liftedVars, indent));
		} else {
			bodyCode = bodyCode.concat(handleStatement(doBody, typeAlias, liftedVars, indent + 1)).concat("\n");
		}
		// evaluate condition expression
		Pair<String, String> evalCond = scanExpressionInDFS(condition, typeAlias, tempVarTable, liftedVars);
		reinitializeVarMap(tempVarTable);
		bodyCode = bodyCode.concat(insertCodeWithIndent(indentStrInner, evalCond.getFirst()));
		
		// restore here
		String initCode = "";
		for(String tempVar : tempVarInScope) {
			initCode = initCode.concat(indentStrInner).concat(mpz_t).concat(" ").concat(tempVar).concat(";\n");
			initCode = initCode.concat(indentStrInner).concat(String.format(mpz_init, tempVar)).concat("\n");
		}
		newCode = newCode.concat(initCode).concat(bodyCode);
		String cleanCode = "";
		for(String localVar : localVariables) {
			cleanCode = cleanCode.concat(indentStrInner).concat(String.format(mpz_clear, localVar)).concat("\n");
		}
		for(String tempVar : tempVarInScope) {
			cleanCode = cleanCode.concat(indentStrInner).concat(String.format(mpz_clear, tempVar)).concat("\n");
		}
		newCode = newCode.concat(cleanCode);
		if(localVariableStack.empty()) {
			localVariables.clear();
		} else {
			copySetFromTo(localVariableStack.pop(), localVariables);
		}
		copySetFromTo(tempVarInScopeBK, tempVarInScope);
		tempVarFrozen.removeAll(tempVarInScope);
		copyMapFromTo(typeAliasBK, typeAlias);
		copySetFromTo(liftedVarsBK, liftedVars);
		copyMapFromTo(signTableBK, signTable);
		// END
		
		newCode = newCode.concat(indentStr).concat("} while(").concat(evalCond.getSecond()).concat(");");
		
		return newCode;
	}
	
	private String handleContinueStatement(IASTContinueStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		String indentStr = String.format(indentBuilder.toString(), "");
		String newCode = indentStr + "continue;";
		return newCode;
	}
	
	private String handleBreakStatement(IASTBreakStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		String indentStr = String.format(indentBuilder.toString(), "");
		String newCode = indentStr + "break;";
		return newCode;
	}
	
	private String handleWhileStatement(IASTWhileStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		String newCode = "";
		IASTStatement whileBody = stmt.getBody();
		IASTExpression condition = stmt.getCondition();
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		StringBuilder indentBuilderInner = new StringBuilder("%1$").append(indent * 2 + 2).append("s");
		String indentStr = String.format(indentBuilder.toString(), "");
		String indentStrInner = String.format(indentBuilderInner.toString(), "");
		
		Pair<String, String> evalCond = scanExpressionInDFS(condition, typeAlias, tempVarTable, liftedVars);
		reinitializeVarMap(tempVarTable);
		newCode = newCode.concat(insertCodeWithIndent(indentStr, evalCond.getFirst()));
		newCode = newCode.concat(indentStr).concat("while(").concat(evalCond.getSecond()).concat(")\n");
		newCode = newCode.concat(indentStr).concat("{\n");
		
		// backup here
		Set<String> localVariablesBK = new HashSet<>();
		Set<String> tempVarInScopeBK = new HashSet<>();
		Map<String, String> typeAliasBK = new HashMap<>();
		Set<String> liftedVarsBK = new HashSet<>();
		Map<String, Boolean> signTableBK = new HashMap<>();
		copySetFromTo(localVariables, localVariablesBK);
		localVariableStack.push(localVariablesBK);
		localVariables.clear();
		tempVarFrozen.addAll(tempVarInScope);
		copySetFromTo(tempVarInScope, tempVarInScopeBK);
		tempVarInScope.clear();
		copyMapFromTo(typeAlias, typeAliasBK);
		copySetFromTo(liftedVars, liftedVarsBK);
		copyMapFromTo(signTable, signTableBK);
		// END
		
		String bodyCode = "";
		if(whileBody instanceof IASTCompoundStatement) {
			nobrace = true;
			bodyCode = bodyCode.concat(handleStatement(whileBody, typeAlias, liftedVars, indent));
		} else {
			bodyCode = bodyCode.concat(handleStatement(whileBody, typeAlias, liftedVars, indent + 1)).concat("\n");
		}
		bodyCode = bodyCode.concat(insertCodeWithIndent(indentStrInner, evalCond.getFirst()));
		
		// restore here
		String initCode = "";
		for(String tempVar : tempVarInScope) {
			initCode = initCode.concat(indentStrInner).concat(mpz_t).concat(" ").concat(tempVar).concat(";\n");
			initCode = initCode.concat(indentStrInner).concat(String.format(mpz_init, tempVar)).concat("\n");
		}
		newCode = newCode.concat(initCode).concat(bodyCode);
		String cleanCode = "";
		for(String localVar : localVariables) {
			cleanCode = cleanCode.concat(indentStrInner).concat(String.format(mpz_clear, localVar)).concat("\n");
		}
		for(String tempVar : tempVarInScope) {
			cleanCode = cleanCode.concat(indentStrInner).concat(String.format(mpz_clear, tempVar)).concat("\n");
		}
		newCode = newCode.concat(cleanCode);
		if(localVariableStack.empty()) {
			localVariables.clear();
		} else {
			copySetFromTo(localVariableStack.pop(), localVariables);
		}
		copySetFromTo(tempVarInScopeBK, tempVarInScope);
		tempVarFrozen.removeAll(tempVarInScope);
		copyMapFromTo(typeAliasBK, typeAlias);
		copySetFromTo(liftedVarsBK, liftedVars);
		copyMapFromTo(signTableBK, signTable);
		// END
		
		newCode = newCode.concat(indentStr).concat("}");
		
		return newCode;
	}
	
	private String handleForStatement(IASTForStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		String newCode = "";
		IASTStatement forBody = stmt.getBody();
		IASTStatement initStmt = stmt.getInitializerStatement();
		IASTExpression condition = stmt.getConditionExpression();
		IASTExpression iteration = stmt.getIterationExpression();
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		String indentControl = indentBuilder.toString();
		StringBuilder indentBuilderInner = new StringBuilder("%1$").append((indent + 1) * 2).append("s");
		String indentControlInner = indentBuilderInner.toString();
		String indentStr = String.format(indentControl, "");
		String indentStrInner = String.format(indentControlInner, "");
		
		// for convenience, we rewrite the for structure with while structure
		// FIRST, initialization statement is executed.
		String initStr = handleStatement(initStmt, typeAlias, liftedVars, indent);
		newCode = newCode.concat(initStr).concat("\n");
		// SECOND, evaluate the condition
		Pair<String, String> evalCond = scanExpressionInDFS(condition, typeAlias, tempVarTable, liftedVars);
		reinitializeVarMap(tempVarTable);
		newCode = newCode.concat(insertCodeWithIndent(indentStr, evalCond.getFirst()));
		newCode = newCode.concat(indentStr).concat("while(").concat(evalCond.getSecond()).concat(")\n");
		newCode = newCode.concat(indentStr).concat("{\n");
		
		// backup here
		Set<String> localVariablesBK = new HashSet<>();
		Set<String> tempVarInScopeBK = new HashSet<>();
		Map<String, String> typeAliasBK = new HashMap<>();
		Set<String> liftedVarsBK = new HashSet<>();
		Map<String, Boolean> signTableBK = new HashMap<>();
		copySetFromTo(localVariables, localVariablesBK);
		localVariableStack.push(localVariablesBK);
		localVariables.clear();
		tempVarFrozen.addAll(tempVarInScope);
		copySetFromTo(tempVarInScope, tempVarInScopeBK);
		tempVarInScope.clear();
		copyMapFromTo(typeAlias, typeAliasBK);
		copySetFromTo(liftedVars, liftedVarsBK);
		copyMapFromTo(signTable, signTableBK);
		// END
		
		String bodyCode = "";
		if(forBody instanceof IASTCompoundStatement) {
			nobrace = true;
			bodyCode = bodyCode.concat(handleStatement(forBody, typeAlias, liftedVars, indent));
		} else {
			bodyCode = bodyCode.concat(handleStatement(forBody, typeAlias, liftedVars, indent + 1)).concat("\n");
		}
		// OK, but we need to evaluate iteration expression first before finishing this loop
		Pair<String, String> evalIter = scanExpressionInDFS(iteration, typeAlias, tempVarTable, liftedVars);
		reinitializeVarMap(tempVarTable);
		// in order to make our program concise, we use a method to insert code
		bodyCode = bodyCode.concat(insertCodeWithIndent(indentStrInner, evalIter.getFirst()));
		bodyCode = bodyCode.concat(insertCodeWithIndent(indentStrInner, evalCond.getFirst()));
		
		// restore here
		String initCode = "";
		for(String tempVar : tempVarInScope) {
			initCode = initCode.concat(indentStrInner).concat(mpz_t).concat(" ").concat(tempVar).concat(";\n");
			initCode = initCode.concat(indentStrInner).concat(String.format(mpz_init, tempVar)).concat("\n");
		}
		newCode = newCode.concat(initCode).concat(bodyCode);
		String cleanCode = "";
		for(String localVar : localVariables) {
			cleanCode = cleanCode.concat(indentStrInner).concat(String.format(mpz_clear, localVar)).concat("\n");
		}
		for(String tempVar : tempVarInScope) {
			cleanCode = cleanCode.concat(indentStrInner).concat(String.format(mpz_clear, tempVar)).concat("\n");
		}
		newCode = newCode.concat(cleanCode);
		if(localVariableStack.empty()) {
			localVariables.clear();
		} else {
			copySetFromTo(localVariableStack.pop(), localVariables);
		}
		copySetFromTo(tempVarInScopeBK, tempVarInScope);
		tempVarFrozen.removeAll(tempVarInScope);
		copyMapFromTo(typeAliasBK, typeAlias);
		copySetFromTo(liftedVarsBK, liftedVars);
		copyMapFromTo(signTableBK, signTable);
		// END
		
		newCode = newCode.concat(indentStr).concat("}");
		
		return newCode;
	}
	
	private String handleIfStatement(IASTIfStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		IASTExpression condition = stmt.getConditionExpression();
		IASTStatement thenClause = stmt.getThenClause();
		IASTStatement elseClause = stmt.getElseClause();
		String newCode = "";
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		String indentControl = indentBuilder.toString();
		String indentStr = String.format(indentControl, "");
		StringBuilder indentInnerBuilder = new StringBuilder("%1$").append((indent + 1) * 2).append("s");
		String indentInnerStr = String.format(indentInnerBuilder.toString(), "");
		
		// FIRST, evaluate the conditional expression, with extra codes before the if-then-else structure
		Pair<String, String> condEval = scanExpressionInDFS(condition, typeAlias, tempVarTable, liftedVars);
		reinitializeVarMap(tempVarTable);
		
		String condCode = condEval.getFirst();
		String condValue = condEval.getSecond();
		newCode = newCode.concat(indentStr).concat(condCode).concat("\n");
		
		// SECOND, construct the structure of if-then-else
		// ** How about if-else if-else if-... structure? We can regard it as if statement is enclosed by else structure
		// NOTE: else branch is optional
		// in order to refine organization of code, we manually add curly braces outside compound statement
		// ** NOTE: (1) for compound statement, we should use the original indent; (2) for other statement, we should add the indent
		newCode = newCode.concat(indentStr).concat("if(").concat(condValue).concat(")").concat("\n");
		newCode = newCode.concat(indentStr).concat("{\n");
		
		// add backup code here
		Set<String> localVariablesBK = new HashSet<>();
		Set<String> tempVarInScopeBK = new HashSet<>();
		Map<String, String> typeAliasBK = new HashMap<>();
		Set<String> liftedVarsBK = new HashSet<>();
		Map<String, Boolean> signTableBK = new HashMap<>();
		copySetFromTo(localVariables, localVariablesBK);
		localVariableStack.push(localVariablesBK);
		localVariables.clear();
		tempVarFrozen.addAll(tempVarInScope);
		copySetFromTo(tempVarInScope, tempVarInScopeBK);
		tempVarInScope.clear();
		copyMapFromTo(typeAlias, typeAliasBK);
		copySetFromTo(liftedVars, liftedVarsBK);
		copyMapFromTo(signTable, signTableBK);
		// END
		
		String thenCode = "";
		if(thenClause instanceof IASTCompoundStatement) {
			nobrace = true;
			thenCode = thenCode.concat(handleStatement(thenClause, typeAlias, liftedVars, indent));
		} else {
			thenCode = thenCode.concat(handleStatement(thenClause, typeAlias, liftedVars, indent + 1)).concat("\n");
		}
		// add restore code here
		String initCode = "";
		for(String tempVar : tempVarInScope) {
			initCode = initCode.concat(indentInnerStr).concat(mpz_t).concat(" ").concat(tempVar).concat(";\n");
			initCode = initCode.concat(indentInnerStr).concat(String.format(mpz_init, tempVar)).concat("\n");
		}
		newCode = newCode.concat(initCode).concat(thenCode);
		String cleanCode = "";
		for(String localVar : localVariables) {
			cleanCode = cleanCode.concat(indentInnerStr).concat(String.format(mpz_clear, localVar)).concat("\n");
		}
		for(String tempVar : tempVarInScope) {
			cleanCode = cleanCode.concat(indentInnerStr).concat(String.format(mpz_clear, tempVar)).concat("\n");
		}
		newCode = newCode.concat(cleanCode);
		if(localVariableStack.empty()) {
			localVariables.clear();
		} else {
			copySetFromTo(localVariableStack.pop(), localVariables);
		}
		copySetFromTo(tempVarInScopeBK, tempVarInScope);
		tempVarFrozen.removeAll(tempVarInScope);
		copyMapFromTo(typeAliasBK, typeAlias);
		copySetFromTo(liftedVarsBK, liftedVars);
		copyMapFromTo(signTableBK, signTable);
		// END
		
		newCode = newCode.concat(indentStr).concat("}\n");
		if(elseClause != null) {
			newCode = newCode.concat(indentStr).concat("else").concat("\n");
			newCode = newCode.concat(indentStr).concat("{\n");
			// back up again
			copySetFromTo(localVariables, localVariablesBK);
			localVariableStack.push(localVariablesBK);
			localVariables.clear();
			tempVarFrozen.addAll(tempVarInScope);
			copySetFromTo(tempVarInScope, tempVarInScopeBK);
			tempVarInScope.clear();
			copyMapFromTo(typeAlias, typeAliasBK);
			copySetFromTo(liftedVars, liftedVarsBK);
			copyMapFromTo(signTable, signTableBK);
			// END
			
			String elseCode = "";
			if(elseClause instanceof IASTCompoundStatement) {
				nobrace = true;
				elseCode = elseCode.concat(handleStatement(elseClause, typeAlias, liftedVars, indent)); 
			} else {
				elseCode = elseCode.concat(handleStatement(elseClause, typeAlias, liftedVars, indent + 1)).concat("\n");
			}
			// restore again
			initCode = "";
			for(String tempVar : tempVarInScope) {
				initCode = initCode.concat(indentInnerStr).concat(mpz_t).concat(" ").concat(tempVar).concat(";\n");
				initCode = initCode.concat(indentInnerStr).concat(String.format(mpz_init, tempVar)).concat("\n");
			}
			newCode = newCode.concat(initCode).concat(elseCode);
			cleanCode = "";
			for(String localVar : localVariables) {
				cleanCode = cleanCode.concat(indentInnerStr).concat(String.format(mpz_clear, localVar)).concat("\n");
			}
			for(String tempVar : tempVarInScope) {
				cleanCode = cleanCode.concat(indentInnerStr).concat(String.format(mpz_clear, tempVar)).concat("\n");
			}
			newCode = newCode.concat(cleanCode);
			if(localVariableStack.empty()) {
				localVariables.clear();
			} else {
				copySetFromTo(localVariableStack.pop(), localVariables);
			}
			copySetFromTo(tempVarInScopeBK, tempVarInScope);
			tempVarFrozen.removeAll(tempVarInScope);
			copyMapFromTo(typeAliasBK, typeAlias);
			copySetFromTo(liftedVarsBK, liftedVars);
			copyMapFromTo(signTableBK, signTable);
			// END
			
			newCode = newCode.concat(indentStr).concat("}\n");
		}
		
		return newCode;
	}
	
	private String handleCompoundStatement(IASTCompoundStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		/*
		 * a compound statement is a curly braced block consisting of statements, we can find it in:
		 * (1) body of function definition (necessary)
		 * (2) blocks inside function (In pure C, it is allowed to place a block in any position of function body)
		 */
		String newCode = "";
		StringBuilder indentBuilder = new StringBuilder("%1$").append((indent + 1) * 2).append("s");
		String indentStr = String.format(indentBuilder.toString(), "");
		boolean needClean = true;
		boolean nobraceBK = nobrace;
		nobrace = false; // restore to the default value. For special cases we will specify 'nobrace' to TRUE
		
		// First of all, we save current context information into stack and start a new one
		// one is responsible to maintain data backup and restore if it controls curly braces of this scope
		Set<String> localVariablesBK = new HashSet<>();
		Set<String> tempVarInScopeBK = new HashSet<>();
		Map<String, String> typeAliasBK = new HashMap<>();
		Set<String> liftedVarsBK = new HashSet<>();
		Map<String, Boolean> signTableBK = new HashMap<>();
		if(!nobraceBK) {
			copySetFromTo(localVariables, localVariablesBK);
			localVariableStack.push(localVariablesBK);
			localVariables.clear();
			//!!! In order to prevent potential leak, we should limit the temporary variables used should be distinct from the ones in parent scope
			tempVarFrozen.addAll(tempVarInScope);
			copySetFromTo(tempVarInScope, tempVarInScopeBK);
			tempVarInScope.clear();
			// we should also back up type alias and lifted variables
			copyMapFromTo(typeAlias, typeAliasBK);
			copySetFromTo(liftedVars, liftedVarsBK);
			copyMapFromTo(signTable, signTableBK);
		}
		
		// OK, we handle these statements sequentially
		IASTStatement[] statementList = stmt.getStatements();
		for(int i = 0; i < statementList.length - 1; i++) {
			IASTStatement statement = statementList[i];
			newCode = newCode.concat(handleStatement(statement, typeAlias, liftedVars, indent + 1)).concat("\n");
		}
		if(statementList.length > 0) {
			IASTStatement finalStatement = statementList[statementList.length - 1];
			if(finalStatement instanceof IASTReturnStatement) {
				needClean = false;
			}
			newCode = newCode.concat(handleStatement(finalStatement, typeAlias, liftedVars, indent + 1)).concat("\n");
		}
		
		if(!nobraceBK) {
			// Then, we should add mpz_init and mpz_clear for these temporary variables
			String initCode = "";
			for(String tempVar : tempVarInScope) {
				initCode = initCode.concat(indentStr).concat(mpz_t).concat(" ").concat(tempVar).concat(";\n");
				initCode = initCode.concat(indentStr).concat(String.format(mpz_init, tempVar)).concat("\n");
			}
			newCode = initCode.concat(newCode);
			
			// Wait... clear these temporary variables to prevent memory leak...
			if(needClean) {
				// two parts: (1) local variables that declared in this scope; (2) temporary variables declared and used in this scope
				String cleanCode = "";
				for(String localVar : localVariables) {
					cleanCode = cleanCode.concat(indentStr).concat(String.format(mpz_clear, localVar)).concat("\n");
				}
				
				for(String tempVar : tempVarInScope) {
					cleanCode = cleanCode.concat(indentStr).concat(String.format(mpz_clear, tempVar)).concat("\n");
				}
				
				newCode = newCode.concat(cleanCode);
			}
			// whether or not the last statement is return, we should restore the context of tempVarInScope and localVariables
			// **NOTE: temporary variables are served for one statement, and can be shared in multiple expressions. Therefore, temporary variables in different scopes
			//         are irrelevant. We save temporary variables in stacks in order to clear ALL allocated GMP integers before this point.
			if(localVariableStack.empty()) {
				localVariables.clear();
			} else {
				copySetFromTo(localVariableStack.pop(), localVariables);;
			}
			copySetFromTo(tempVarInScopeBK, tempVarInScope);
			tempVarFrozen.removeAll(tempVarInScope);
			copyMapFromTo(typeAliasBK, typeAlias);
			copySetFromTo(liftedVarsBK, liftedVars);
			copyMapFromTo(signTableBK, signTable);
		}
		
		
		// since 'nobrace' is a global flag, if we use the modified value through the scope of this compound statement, all other statements will be affected
		if(!nobraceBK) {
			// Then this bunch of statements should be braced
			StringBuilder indentOuter = new StringBuilder("%1$").append(indent * 2 + 1).append("s");
			String indentOuterStr = indentOuter.toString();
			String leftBrace = String.format(indentOuterStr, "{");
			String rightBrace = String.format(indentOuterStr, "}");
			newCode = leftBrace.concat("\n").concat(newCode).concat(rightBrace);
		}
		// **NOTE: after this point, 'nobrace' has default value. Since before adding curly braces we use the backup value which is irrelevant to this 'nobrace',
		//         thus the operation is safe.
		
		return newCode;
	}
	
	private String handleDeclarationStatement(IASTDeclarationStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		String newCode = "";
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		IASTDeclaration declaration = stmt.getDeclaration();
		if(declaration instanceof IASTSimpleDeclaration) {
			String newStmt = handleSimpleDeclaration((IASTSimpleDeclaration)declaration, typeAlias, liftedVars, false);
			newCode = newCode.concat(String.format(indentBuilder.toString(), "")).concat(newStmt);
			return newCode;
		} else {
			// IASTASMDeclaration, IASTProblemDeclaration
			// NOTE: raw signature of this statement does not include spaces of indent
			// Function definition is impossible inside a function body
			newCode = newCode.concat(String.format(indentBuilder.toString(), "")).concat(stmt.getRawSignature());
			return newCode;
		}
	}
	
	private String handleExpressionStatement(IASTExpressionStatement stmt, Map<String, String> typeAlias, Set<String> liftedVars, int indent) {
		// one of the most common statements in C program
		IASTExpression expression = stmt.getExpression();
		
		StringBuilder indentBuilder = new StringBuilder("%1$").append(indent * 2).append("s");
		/*
		 * IASTExpression has many different types, we should carefully handle these cases!
		 * In order to introduce as the fewest intermediate variables as possible, we need to traverse this expression tree first and find the deepest node to start conversion
		 * We can prove: if a node in expression tree can have at most N children, it is sufficient to use (2N-1) intermediate variables (to be proved)
		 */
		
		// lifted variables should be added into this table since for now it is impossible to update the value of program variables
		// SOLUTION: rewrite the condition "tempVarMap.containsKey(opName)" as "isHighPrecisionVar(opName, tempVarMap, liftedVars)", since the sources of highly-precise value are: (1) temporary GMP integer; (2) original numerical variable
		
		// FIRST, convert this expression to GMP version, using DFS to traverse expression
		// **why we ignore the second component of result? For an expression statement, the result of the top expression has no effects on program state, such as change the value of a variable.
		// the second component is necessary when the top expression is a function call
		Pair<String, String> evalExpr = scanExpressionInDFS(expression, typeAlias, tempVarTable, liftedVars);
		String newCode = evalExpr.getFirst();
		String exprVal = evalExpr.getSecond();
		
		// SECOND, some clean work
		// the table of temporary variable could be extended!
		reinitializeVarMap(tempVarTable);
		
		// setting up indent before returning
		newCode = String.format(indentBuilder.toString(), "") + newCode;
		if(expression instanceof IASTFunctionCallExpression) {
			newCode = newCode.concat("\n").concat(String.format(indentBuilder.toString(), "")).concat(exprVal).concat(";");
		}
		
		return newCode;
	}
	
	// developers are responsible to add semicolon at the end of the statement
	private String insertCodeWithIndent(String indentString, String code) {
		if(code.isEmpty()) {
			return "";
		} else {
			return (indentString + code + "\n");
		}
	}
	
	private boolean isHighPrecisionVar(String operandName, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		if(tempVarMap.containsKey(operandName)) {
			return true;
		}
		if(operandName.length() < tempPfLen) {
			return false; // that's impossible to be an original integer variable
		} else {
			String protoName = operandName.substring(tempPfLen);
			if(liftedVars.contains(protoName)) {
				return true;
			} else {
				return false;
			}
		}
	}
	
	private boolean isLiftedVar(String operandName, Set<String> liftedVars) {
		if(operandName.length() < tempPfLen) {
			return false; // that's impossible to be an original integer variable
		} else {
			String protoName = operandName.substring(tempPfLen);
			if(liftedVars.contains(protoName)) {
				return true;
			} else {
				return false;
			}
		}
	}
	
	/*
	 * Pair<S1, S2>:
	 * S1: generated code of expression
	 * S2: the name of temporary variable admit by this expression
	 */
	private Pair<String, String> scanExpressionInDFS(IASTExpression expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		// handle this expression in different cases
		if(expression instanceof IASTArraySubscriptExpression) {
			return scanArraySubscriptExpression((IASTArraySubscriptExpression)expression, typeAlias, tempVarMap, liftedVars);
		} else if(expression instanceof IASTBinaryExpression) {
			return scanBinaryExpression((IASTBinaryExpression)expression, typeAlias, tempVarMap, liftedVars);
		} else if(expression instanceof IASTUnaryExpression) {
			return scanUnaryExpression((IASTUnaryExpression)expression, typeAlias, tempVarMap, liftedVars);
		} else if(expression instanceof IASTConditionalExpression) {
			return scanConditionalExpression((IASTConditionalExpression)expression, typeAlias, tempVarMap, liftedVars);
		} else if(expression instanceof IASTFieldReference) {
			return scanFieldReference((IASTFieldReference)expression, typeAlias, tempVarMap, liftedVars);
		} else if(expression instanceof IASTFunctionCallExpression) {
			return scanFunctionCallExpression((IASTFunctionCallExpression)expression, typeAlias, tempVarMap, liftedVars);
		} else if(expression instanceof IASTIdExpression) {
			return scanIdExpression((IASTIdExpression)expression, typeAlias, tempVarMap, liftedVars);
		} else if(expression instanceof IASTLiteralExpression) {
			return scanLiteralExpression((IASTLiteralExpression)expression, typeAlias, tempVarMap, liftedVars);
		} else if(expression instanceof IASTCastExpression) {
			return scanCastExpression((IASTCastExpression)expression, typeAlias, tempVarMap, liftedVars);
		} else {
			// unexpected case, we just return current expression directly
			if(expression != null) {
				String newCode = expression.getRawSignature();
				return Pair.of("", newCode);
			} else {
				return Pair.of("", "");
			}
		}
	}
	
	private Pair<String, String> scanArraySubscriptExpression(IASTArraySubscriptExpression expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		IASTExpression arrayExpr = expression.getArrayExpression();
		IASTExpression subscriptExpr = (IASTExpression)expression.getArgument();
		// in this case, "arrayExpr" keeps unchanged while "subscriptExpr" should be further checked before to be used as index
		String newCode = "";
		String newName = "";
		Pair<String, String> eval1 = scanExpressionInDFS(arrayExpr, typeAlias, tempVarMap, liftedVars);
		Pair<String, String> eval2 = scanExpressionInDFS(subscriptExpr, typeAlias, tempVarMap, liftedVars);
		newCode = newCode.concat(eval1.getFirst()).concat(eval2.getFirst());
		// FIRST, check if subscriptExpr is in the range of unsigned int
		// (1) subscriptExpr is a GMP number; (2) is not a GMP number, just use it directly
		String subscriptName = eval2.getSecond();
		if(isHighPrecisionVar(subscriptName, tempVarMap, liftedVars)) {
			// GMP number
			newName = eval1.getSecond() + "[" + String.format(check_gmp_uint, subscriptName) + "]";
		} else {
			Pair<Integer, Boolean> subType = getFinalType(subscriptExpr, typeAlias);
			String signed = subType.getSecond() ? "1" : "0";
			newName = eval1.getSecond() + "[" + String.format(check_uint, subscriptName, signed) + "]";
		}
		
		// return the result
		return Pair.of(newCode, newName);
	}
	
	private Pair<String, String> scanBinaryExpression(IASTBinaryExpression expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		IASTExpression operand1 = expression.getOperand1();
		IASTExpression operand2 = expression.getOperand2();
		String newCode = "";
		String newName = "";
		Map<String, Boolean> tempMapMirror = new HashMap<String, Boolean>();
		copyMapFromTo(tempVarMap, tempMapMirror);
		Pair<String, String> eval1 = scanExpressionInDFS(operand1, typeAlias, tempVarMap, liftedVars);
		// After evaluating the left operand, check how the temporary variable map changes.
		String[] delta1 = getMapDelta(tempVarMap, tempMapMirror);
		
		copyMapFromTo(tempVarMap, tempMapMirror);
		Pair<String, String> eval2 = scanExpressionInDFS(operand2, typeAlias, tempVarMap, liftedVars);
		String[] delta2 = getMapDelta(tempVarMap, tempMapMirror);
		
		newCode = newCode.concat(eval1.getFirst()).concat(eval2.getFirst());
		String op1Name = eval1.getSecond();
		String op2Name = eval2.getSecond();
		
		// FIRST, decide kind of operator
		int operator = expression.getOperator();
		// SECOND, handle different cases
		switch(operator) {
		case IASTBinaryExpression.op_assign: {
			Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
			Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
			// CASE 1: l-value is a GMP integer
			// in fact we should pre-process the expression before handling, for example, drop unnecessary brackets
			if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
				if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
					newCode = newCode.concat(String.format(mpz_set, op1Name, op2Name)).concat("; ");
					newName = "1";
					// since newName is constant without any temp variable, we can release all temp vars used in operand1
					recycleVars(delta2, tempVarMap);
				} else {
					// r-value is a ordinary numerical type
					if(rtype.getSecond() == false && rtype.getFirst() == 8) {
						newCode = newCode.concat(String.format(mpz_set_ui, op1Name, op2Name)).concat("; ");
					} else {
						newCode = newCode.concat(String.format(mpz_set_si, op1Name, op2Name)).concat("; ");
					}
					newName = "1";
				}
				recycleVars(delta1, tempVarMap);
				
				// if we perform p = q where p is a high precision variable, q is an expression and p is pointed by a pointer s,
				//    we should update the ordinary version since *s can only visit ordinary variable.
				if(pointerInfo.containsValue(op1Name)) {
					// thus we should update its ordinary variable
					String origName = op1Name.substring(tempPfLen);
					int llength = ltype.getFirst();
					boolean lsign = ltype.getSecond();
					if(lsign) {
						if(llength == 1) {
							newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_schar, op1Name)).concat("; ");
						} else if(llength == 2) {
							newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_sshort, op1Name)).concat("; ");
						} else if(llength == 4) {
							newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_int, op1Name)).concat("; ");
						} else {
							newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_slong, op1Name)).concat("; ");
						}
					} else {
						if(llength == 1) {
							newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_uchar, op1Name)).concat("; ");
						} else if(llength == 2) {
							newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_ushort, op1Name)).concat("; ");
						} else if(llength == 4) {
							newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_uint, op1Name)).concat("; ");
						} else {
							newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_ulong, op1Name)).concat("; ");
						}
					}
				}
				
			} else {
				// l-value is not a GMP integer
				if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
					// but r-value is a GMP integer: range check is required
					Pair<Integer, Boolean> finalType = getFinalType(operand1, typeAlias);
					int typeLength = finalType.getFirst();
					boolean typeSign = finalType.getSecond();
					if(typeSign) {
						// typeSign == true, it is a signed value
						if(typeLength == 1) {
							newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_schar, op2Name)).concat("; ");
						} else if(typeLength == 2) {
							newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_sshort, op2Name)).concat("; ");
						} else if(typeLength == 4) {
							newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_int, op2Name)).concat("; ");
						} else {
							newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_slong, op2Name)).concat("; ");
						}
					} else {
						// unsigned value
						if(typeLength == 1) {
							newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_uchar, op2Name)).concat("; ");
						} else if(typeLength == 2) {
							newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_ushort, op2Name)).concat("; ");
						} else if(typeLength == 4) {
							newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_uint, op2Name)).concat("; ");
						} else {
							newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_ulong, op2Name)).concat("; ");
						}
					}
					newName = "1";
					recycleVars(delta2, tempVarMap);
				} else {
					// neither l-value nor r-value are GMP integer
					// NOTE: it is possible that both sides are not numerical values, then we just output them directly
					int llength = ltype.getFirst();
					int rlength = rtype.getFirst();
					boolean lsign = ltype.getSecond();
					String rsign = rtype.getSecond() ? "1" : "0";
					if(llength == -1 || rlength == -1) {
						newCode = newCode.concat(op1Name).concat(" = ").concat(op2Name).concat("; ");
						
						// it is possible that two operands are pointers. We should maintain pointer relations here
						// only two types of expressions are supported:
						// (1) p = q
						// (2) p = &q
						// (3) p = ***q
						// **NOTE: YES! We cannot apply multiple &s on one operand since &q is not an l-value any longer!
						if(operand1 instanceof IASTIdExpression) {
							if(operand2 instanceof IASTIdExpression) {
								// case 1
								String pointerVal = pointerInfo.get(op2Name);
								if(pointerVal != null) {
									pointerInfo.put(op1Name, pointerVal);
								}
							} else if(operand2 instanceof IASTUnaryExpression) {
								IASTUnaryExpression possibleRefExpr = (IASTUnaryExpression)operand2;
								int possibleAmper = possibleRefExpr.getOperator();
								if(possibleAmper == IASTUnaryExpression.op_amper) {
									String lvalue = op2Name.substring(1);
									// two cases: (1) existing pointer name; (2) original name of a local integer variable
									String newlvalue = prefix + lvalue;
									if(isHighPrecisionVar(newlvalue, tempVarMap, liftedVars)) {
										pointerInfo.put(op1Name, newlvalue);
									}
									if(pointerInfo.containsKey(lvalue)) {
										pointerInfo.put(op1Name, lvalue);
									}
								} else if(possibleAmper == IASTUnaryExpression.op_star) {
									String starVal = op2Name;
									int refLevel = 0;
									while(starVal.charAt(0) == '*') {
										starVal = starVal.substring(1);
										starVal = deBracket(starVal);
										refLevel++;
									}
									while(refLevel > 0) {
										starVal = pointerInfo.get(starVal);
										if(starVal == null) {
											break;
										}
										refLevel--;
									}
									if(refLevel == 0) {
										starVal = pointerInfo.get(starVal);
										if(starVal != null) {
											pointerInfo.put(op1Name, starVal);
										}
									}
								}
							}
						}
						
					} else {
						// numerical assignment
						if(lsign) {
							if(llength == 1) {
								newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_schar, op2Name, rsign)).concat("; ");
							} else if(llength == 2) {
								newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_sshort, op2Name, rsign)).concat("; ");
							} else if(llength == 4) {
								newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_int, op2Name, rsign)).concat("; ");
							} else {
								newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_slong, op2Name, rsign)).concat("; ");
							}
						} else {
							if(llength == 1) {
								newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_uchar, op2Name, rsign)).concat("; ");
							} else if(llength == 2) {
								newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_ushort, op2Name, rsign)).concat("; ");
							} else if(llength == 4) {
								newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_uint, op2Name, rsign)).concat("; ");
							} else {
								newCode = newCode.concat(op1Name).concat(" = ").concat(String.format(check_ulong, op2Name, rsign)).concat("; ");
							}
						}
						
						// l-value is not a GMP integer, thus it can be a STAR expression. Thus, we can modify value of variable through pointer operations.
						if(operand1 instanceof IASTUnaryExpression) {
							IASTUnaryExpression possibleStarExpr = (IASTUnaryExpression)operand1;
							int possibleStarOperator = possibleStarExpr.getOperator();
							if(possibleStarOperator == IASTUnaryExpression.op_star) {
								// A star expression can possibly include several stars (>= 1 star)
								String derefVal = op1Name;
								int derefLevel = 0;
								while(derefVal.charAt(0) == '*') {
									derefVal = derefVal.substring(1);
									derefLevel++;
									derefVal = deBracket(derefVal);
								}
								// now, the unpacked variable refers to a pointer, however we need to do something unless its dereference refers to a GMP variable
								while(derefLevel > 0) {
									derefVal = pointerInfo.get(derefVal);
									if(derefVal == null) {
										break;
									}
									derefLevel--;
								}
								// check again and update this one!
								if(derefLevel == 0) {
									if(isHighPrecisionVar(derefVal, tempVarMap, liftedVars)) {
										// Then, we are trying to modify a GMP variable via its pointer!!
										// check whether right operand is a GMP variable
										if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
											newCode = newCode.concat(String.format(mpz_set, derefVal, op2Name)).concat("; ");
										} else if (rtype.getFirst() != -1) {
											// then this is a numerical value, not a pointer or some other things.
											if(rtype.getSecond() == false) {
												newCode = newCode.concat(String.format(mpz_set_ui, derefVal, op2Name)).concat("; ");
											} else {
												newCode = newCode.concat(String.format(mpz_set_si, derefVal, op2Name)).concat("; ");
											}
										}
									}
								}
							}
						}
					}
					newName = "1";
				}
				
			}
			break;
		}
		case IASTBinaryExpression.op_binaryAnd: {
			// Since this is a bit-manipulation, it is unnecessary to use GMP integer, we first cast GMP integer to normal one then calculating AND
			newName = handleBitOperation(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "&");
			break;
		}
		case IASTBinaryExpression.op_binaryAndAssign: {
			String addCode = handleBitOperationWithAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "&");
			newCode = newCode.concat(addCode);
			newName = "1";
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_binaryOr: {
			// Copy from binary_and with the only modification of logical operator
			newName = handleBitOperation(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "|");
			break;
		}
		case IASTBinaryExpression.op_binaryOrAssign: {
			// Copy from op_binaryAndAssign with the only modification of logical operator
			String addCode = handleBitOperationWithAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "|");
			newCode = newCode.concat(addCode);
			newName = "1";
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_binaryXor: {
			// Copy from binary_and with the only modification of logical operator
			newName = handleBitOperation(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "^");
			break;
		}
		case IASTBinaryExpression.op_binaryXorAssign: {
			// Copy from op_binaryAndAssign with the only modification of logical operator
			String addCode = handleBitOperationWithAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "^");
			newCode = newCode.concat(addCode);
			newName = "1";
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_equals: {
			// We can handle other predicate operations similarly
			// ATTENTION! operand can be numerical numbers or POINTERS! In gcc, pointers can be implicitly casted to a number of long type
			Pair<String, String> newContent = handleComparisonPredicate(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "==");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_notequals: {
			Pair<String, String> newContent = handleComparisonPredicate(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "!=");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_greaterEqual: {
			Pair<String, String> newContent = handleComparisonPredicate(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, ">=");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_greaterThan: {
			Pair<String, String> newContent = handleComparisonPredicate(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, ">");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_lessEqual: {
			Pair<String, String> newContent = handleComparisonPredicate(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "<=");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_lessThan: {
			Pair<String, String> newContent = handleComparisonPredicate(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "<");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_logicalAnd: {
			Pair<String, String> newContent = handleLogicalArithmetic(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "||");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_logicalOr: {
			Pair<String, String> newContent = handleLogicalArithmetic(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "&&");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_plus: {
			// WARNING: in integer arithmetic operations, plus and minus are special. This is because its value could be numerical one or pointer!
			// RULE: P + N :: P; N + N :: N; P + P :: N
			Pair<String, String> newContent = handlePlusAndMinus(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "+");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_minus: {
			Pair<String, String> newContent = handlePlusAndMinus(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "-");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_plusAssign: {
			// RULE: P += N , (N+P) :: P; N += P, (P+N) :: N
			String addCode = handlePlusAndMinusAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "+");
			newCode = newCode.concat(addCode);
			newName = "1"; // please clear all temporary variables in the function above
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_minusAssign: {
			String addCode = handlePlusAndMinusAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "-");
			newCode = newCode.concat(addCode);
			newName = "1"; // please clear all temporary variables in the function above
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_multiply: {
			// both operands should be numerical values
			Pair<String, String> newContent = handleBinaryOperation(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "*");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_divide: {
			Pair<String, String> newContent = handleBinaryOperation(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "/");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_modulo: {
			Pair<String, String> newContent = handleBinaryOperation(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "%");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_shiftLeft: {
			// shift operation is special. Unlike other integer arithmetic operations, there are requirements for operands
			Pair<String, String> newContent = handleShiftOperation(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "<<");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_shiftRight: {
			Pair<String, String> newContent = handleShiftOperation(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, ">>");
			newCode = newCode.concat(newContent.getFirst());
			newName = newContent.getSecond();
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_multiplyAssign: {
			String addCode = handleBinaryOperationAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "*");
			newCode = newCode.concat(addCode);
			newName = "1";
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_divideAssign: {
			String addCode = handleBinaryOperationAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "/");
			newCode = newCode.concat(addCode);
			newName = "1";
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_moduloAssign: {
			String addCode = handleBinaryOperationAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "%");
			newCode = newCode.concat(addCode);
			newName = "1";
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_shiftLeftAssign: {
			String addCode = handleShiftOperationAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, "<<");
			newCode = newCode.concat(addCode);
			newName = "1";
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		case IASTBinaryExpression.op_shiftRightAssign: {
			String addCode = handleShiftOperationAssign(operand1, operand2, op1Name, op2Name, tempVarMap, liftedVars, typeAlias, ">>");
			newCode = newCode.concat(addCode);
			newName = "1";
			recycleVars(delta1, tempVarMap);
			recycleVars(delta2, tempVarMap);
			break;
		}
		default: {
			// keep this expression for robustness
			newCode = "";
			newName = expression.getRawSignature();
		}
		}
		
		return Pair.of(newCode, newName);
	}
	
	private String deBracket(String str) {
		String newStr = str;
		while(newStr.charAt(0) == '(' && newStr.charAt(newStr.length() - 1) == ')') {
			newStr = newStr.substring(1, newStr.length() - 1);
		}
		return newStr;
	}
	
	private Pair<String, String> scanUnaryExpression(IASTUnaryExpression expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		IASTExpression operand = expression.getOperand();
		String newCode = "";
		String newName = "";
		
		Map<String, Boolean> tempMapMirror = new HashMap<String, Boolean>();
		copyMapFromTo(tempVarMap, tempMapMirror);
		
		// NOTE: two operands cannot include the same intermediate variable
		Pair<String, String> eval = scanExpressionInDFS(operand, typeAlias, tempVarMap, liftedVars);
		String[] delta = getMapDelta(tempVarMap, tempMapMirror);
		
		newCode = newCode.concat(eval.getFirst());
		String opName = eval.getSecond();
		
		int operator = expression.getOperator();
		switch(operator) {
		case IASTUnaryExpression.op_bracketedPrimary: {
			// bracket does nothing on changing semantics of expression
			// DO NOT convert a possible integer into a GMP integer since it is a problematic operation for some expressions such as array subscript
			// NOTE: bracketed expression can be either l-value or r-value
			newName = opName;
			break;
		}
		case IASTUnaryExpression.op_minus: {
			// negative of this expression
			// this expression can only be r-value, thus we can convert it into GMP value
			String newTempVar = getVar(tempVarMap);
			if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
				newCode = newCode.concat(String.format(mpz_neg, newTempVar, opName)).concat("; ");
				newName = newTempVar;
				recycleVar(opName, tempVarMap);
			} else {
				Pair<Integer, Boolean> type = getFinalType(operand, typeAlias);
				if((type.getSecond() == false && type.getFirst() == 8) || type.getFirst() == -1) {
					newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, opName)).concat("; ");
				} else {
					newCode = newCode.concat(String.format(mpz_set_si, newTempVar, opName)).concat("; ");
				}
				newCode = newCode.concat(String.format(mpz_neg, newTempVar, newTempVar)).concat("; ");
				newName = newTempVar;
				recycleVars(delta, tempVarMap);
			}
			break;
		}
		case IASTUnaryExpression.op_plus: {
			// do nothing
			// if possible, try to convert a non-GMP integer to GMP integer since this expression can only be r-value
			if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
				newName = opName;
			} else {
				String newTempVar = getVar(tempVarMap);
				Pair<Integer, Boolean> type = getFinalType(operand, typeAlias);
				if((type.getSecond() == false && type.getFirst() == 8) || type.getFirst() == -1) {
					newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, opName)).concat("; ");
				} else {
					newCode = newCode.concat(String.format(mpz_set_si, newTempVar, opName)).concat("; ");
				}
				newName = newTempVar;
				recycleVars(delta, tempVarMap);
			}
			break;
		}
		case IASTUnaryExpression.op_not: {
			// NOT typically operates on boolean value. If the operand is 0, then the expression is valued 1; if the operand is non-zero value, then the expression is valued 0
			// NOTE: this expression can only be r-value
			String newTempVar = getVar(tempVarMap);
			if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
				// we change the value of operand, if operand is a local variable, then its value is accidentally altered
				newCode = newCode.concat(String.format(mpz_set_si, newTempVar, String.format(mpz_cmp, opName, "0"))).concat("; ");
				newName = String.format(mpz_cmp, newTempVar, "0") + " == 0 ? 1 : 0";
			} else {
				Pair<Integer, Boolean> type = getFinalType(operand, typeAlias);
				if((type.getSecond() == false && type.getFirst() == 8) || type.getFirst() == -1) {
					newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, opName)).concat("; ");
				} else {
					newCode = newCode.concat(String.format(mpz_set_si, newTempVar, opName)).concat("; ");
				}
				newCode = newCode.concat(String.format(mpz_set_si, newTempVar, String.format(mpz_cmp, newTempVar, "0"))).concat("; ");
				newName = String.format(mpz_cmp, newTempVar, "0") + " == 0 ? 1 : 0";
			}
			recycleVars(delta, tempVarMap);
			break;
		}
		case IASTUnaryExpression.op_postFixDecr: {
			// the value of this expression is the same as the original, but its operand decrements
			// example: a-- :: (1) a; (2) a = a - 1
			// since this operation is involved with assignment, we have to handle this with great care
			// NOTE: this expression can only be r-value, moreover, its operand can be only l-value!

			// 2 cases: (1) operand is a pointer; (2) operand is a numerical value
			Pair<Integer, Boolean> type = getFinalType(operand, typeAlias);
			if(type.getFirst() == -1 && (operand.getExpressionType() instanceof IPointerType)) {
				// keep unchanged
				newName = opName + "--";
			} else {
				// numerical value
				int olength = type.getFirst();
				boolean osign = type.getSecond();
				String newTempVar = getVar(tempVarMap);
				if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
					newCode = newCode.concat(String.format(mpz_set, newTempVar, opName)).concat("; ");
					newName = newTempVar;
					newCode = newCode.concat(String.format(mpz_sub_ui, opName, opName, "1")).concat("; ");
					recycleVars(delta, tempVarMap);
				} else {
					// FIRST, convert operand into GMP integer and get the result
					if((osign == false && olength == 8) || olength == -1) {
						newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, opName)).concat("; ");
					} else {
						newCode = newCode.concat(String.format(mpz_set_si, newTempVar, opName)).concat("; ");
					}
					// SECOND, compute the result
					newCode = newCode.concat(String.format(mpz_sub_ui, newTempVar, newTempVar, "1")).concat("; ");
					// THIRD, pass the result back to the operand
					if(osign) {
						if(olength == 1) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_schar, newTempVar)).concat("; ");
						} else if(olength == 2) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_sshort, newTempVar)).concat("; ");
						} else if(olength == 4) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_int, newTempVar)).concat("; ");
						} else {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_slong, newTempVar)).concat("; ");
						}
					} else {
						if(olength == 1) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_uchar, newTempVar)).concat("; ");
						} else if(olength == 2) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_ushort, newTempVar)).concat("; ");
						} else if(olength == 4) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_uint, newTempVar)).concat("; ");
						} else {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_ulong, newTempVar)).concat("; ");
						}
					}
					newName = opName + " + 1"; // keep the original value
				}
			}
			break;
		}
		case IASTUnaryExpression.op_postFixIncr: {
			Pair<Integer, Boolean> type = getFinalType(operand, typeAlias);
			if(type.getFirst() == -1 && (operand.getExpressionType() instanceof IPointerType)) {
				// keep unchanged
				newName = opName + "++";
			} else {
				// numerical value
				int olength = type.getFirst();
				boolean osign = type.getSecond();
				String newTempVar = getVar(tempVarMap);
				if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
					newCode = newCode.concat(String.format(mpz_set, newTempVar, opName)).concat("; ");
					newName = newTempVar;
					newCode = newCode.concat(String.format(mpz_add_ui, opName, opName, "1")).concat("; ");
					recycleVars(delta, tempVarMap);
				} else {
					// FIRST, convert operand into GMP integer and get the result
					if((osign == false && olength == 8) || olength == -1) {
						newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, opName)).concat("; ");
					} else {
						newCode = newCode.concat(String.format(mpz_set_si, newTempVar, opName)).concat("; ");
					}
					// SECOND, compute the result
					newCode = newCode.concat(String.format(mpz_add_ui, newTempVar, newTempVar, "1")).concat("; ");
					// THIRD, pass the result back to the operand
					if(osign) {
						if(olength == 1) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_schar, newTempVar)).concat("; ");
						} else if(olength == 2) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_sshort, newTempVar)).concat("; ");
						} else if(olength == 4) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_int, newTempVar)).concat("; ");
						} else {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_slong, newTempVar)).concat("; ");
						}
					} else {
						if(olength == 1) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_uchar, newTempVar)).concat("; ");
						} else if(olength == 2) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_ushort, newTempVar)).concat("; ");
						} else if(olength == 4) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_uint, newTempVar)).concat("; ");
						} else {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_ulong, newTempVar)).concat("; ");
						}
					}
					newName = opName + " - 1"; // keep the original value
				}
			}
			break;
		}
		case IASTUnaryExpression.op_prefixDecr: {
			// Unlike the previous case, the operand is changed first and then pass its updated value for further use
			// NOTE: this expression is also r-value, and its operand is l-value
			Pair<Integer, Boolean> type = getFinalType(operand, typeAlias);
			if(type.getFirst() == -1 && (operand.getExpressionType() instanceof IPointerType)) {
				// stay unchanged
				newName = "--" + opName;
			} else {
				int olength = type.getFirst();
				boolean osign = type.getSecond();
				// no intermediate variable is required because we can directly use the updated value
				if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
					newCode = newCode.concat(String.format(mpz_sub_ui, opName, opName, "1")).concat("; ");
					newName = opName;
				} else {
					String newTempVar = getVar(tempVarMap);
					if((osign == false && olength == 8) || olength == -1) {
						newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, opName)).concat("; ");
					} else {
						newCode = newCode.concat(String.format(mpz_set_si, newTempVar, opName)).concat("; ");
					}
					newCode = newCode.concat(String.format(mpz_sub_ui, newTempVar, newTempVar, "1")).concat("; ");
					if(osign) {
						if(olength == 1) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_schar, newTempVar)).concat("; ");
						} else if(olength == 2) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_sshort, newTempVar)).concat("; ");
						} else if(olength == 4) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_int, newTempVar)).concat("; ");
						} else {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_slong, newTempVar)).concat("; ");
						}
					} else {
						if(olength == 1) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_uchar, newTempVar)).concat("; ");
						} else if(olength == 2) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_ushort, newTempVar)).concat("; ");
						} else if(olength == 4) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_uint, newTempVar)).concat("; ");
						} else {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_ulong, newTempVar)).concat("; ");
						}
					}
					newName = newTempVar;
					recycleVars(delta, tempVarMap);
				}
			}
			break;
		}
		case IASTUnaryExpression.op_prefixIncr: {
			Pair<Integer, Boolean> type = getFinalType(operand, typeAlias);
			if(type.getFirst() == -1 && (operand.getExpressionType() instanceof IPointerType)) {
				// stay unchanged
				newName = "++" + opName;
			} else {
				int olength = type.getFirst();
				boolean osign = type.getSecond();
				// no intermediate variable is required because we can directly use the updated value
				if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
					newCode = newCode.concat(String.format(mpz_add_ui, opName, opName, "1")).concat("; ");
					newName = opName;
				} else {
					String newTempVar = getVar(tempVarMap);
					if((osign == false && olength == 8) || olength == -1) {
						newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, opName)).concat("; ");
					} else {
						newCode = newCode.concat(String.format(mpz_set_si, newTempVar, opName)).concat("; ");
					}
					newCode = newCode.concat(String.format(mpz_add_ui, newTempVar, newTempVar, "1")).concat("; ");
					if(osign) {
						if(olength == 1) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_schar, newTempVar)).concat("; ");
						} else if(olength == 2) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_sshort, newTempVar)).concat("; ");
						} else if(olength == 4) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_int, newTempVar)).concat("; ");
						} else {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_slong, newTempVar)).concat("; ");
						}
					} else {
						if(olength == 1) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_uchar, newTempVar)).concat("; ");
						} else if(olength == 2) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_ushort, newTempVar)).concat("; ");
						} else if(olength == 4) {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_uint, newTempVar)).concat("; ");
						} else {
							newCode = newCode.concat(opName).concat(" = ").concat(String.format(check_gmp_ulong, newTempVar)).concat("; ");
						}
					}
					newName = newTempVar;
					recycleVars(delta, tempVarMap);
				}
			}
			break;
		}
		case IASTUnaryExpression.op_sizeof: {
			// ** NOTE: operand of sizeof expression can be either l-value or r-value, or even TYPENAME!
			newName = "sizeof(" + operand.getRawSignature() + ")";
			break;
		}
		case IASTUnaryExpression.op_tilde: {
			// bit-wise NOT operation
			// like other bit manipulating operation, overflow is allowed so we will limit the length of numerical value
			Pair<Integer, Boolean> type = getFinalType(operand, typeAlias);
			int olength = type.getFirst();
			String osign = type.getSecond() ? "1" : "0";
			String opStr = "";
			if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
				if(olength == 1) {
					opStr = String.format(check_gmp_uchar, opName);
				} else if(olength == 2) {
					opStr = String.format(check_gmp_ushort, opName);
				} else if(olength == 4) {
					opStr = String.format(check_gmp_uint, opName);
				} else {
					opStr = String.format(check_gmp_ulong, opName);
				}
			} else {
				if(olength == 1) {
					opStr = String.format(check_uchar, opName, osign);
				} else if(olength == 2) {
					opStr = String.format(check_ushort, opName, osign);
				} else if(olength == 4) {
					opStr = String.format(check_uint, opName, osign);
				} else {
					opStr = String.format(check_ulong, opName, osign);
				}
			}
			
			newName = "~(" + opStr + ")";
			break;
		}
		case IASTUnaryExpression.op_amper: {
			// ampersand(&) is used to get the address of a data structure
			// NOTE: the operand of & can be only l-value
			// ** since the operand is l-value, it is impossible to attempt to get the address of a temporary variable
			
			/*
			 * Some comments on our design
			 * 
			 * If a pointer points to local variable which is converted into a GMP number, what should we do? If we don't want to change the semantics of program, 
			 *   we should keep the type of this pointer, i.e. make this pointer point to the same data structure as the original program. The reason is twofold:
			 * (1) a pointer can be regarded as a tiny structure with only one field. Since the type of this field is fixed, the data passed to this field should
			 *     not exceed the limitation of type;
			 * (2) consider function call, such as scanf("%d", &in) where in is a normal integer data in program. However, if we replace all the integers with GMP
			 *     integers, scanf would fail.
			 * 
			 * Our proposed method is to store the value of GMP integer into the corresponding normal variable when encountering ampersand operation. When the
			 *   program (probably in other functions) tries to modify its value, it should use star (*) operator to get its value or update it with new value.
			 * 
			 */
			
			Pair<Integer, Boolean> type = getFinalType(operand, typeAlias);
			int olength = type.getFirst();
			boolean osign = type.getSecond();
			if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
				// the operand must be a local variable
				// STEP 1: copy the value of highly-precise A to original machine integer A'
				String origName = opName.substring(tempPfLen);
				if(osign) {
					if(olength == 1) {
						newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_schar, opName)).concat("; ");
					} else if(olength == 2) {
						newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_sshort, opName)).concat("; ");
					} else if(olength == 4) {
						newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_int, opName)).concat("; ");
					} else {
						newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_slong, opName)).concat("; ");
					}
				} else {
					if(olength == 1) {
						newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_uchar, opName)).concat("; ");
					} else if(olength == 2) {
						newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_ushort, opName)).concat("; ");
					} else if(olength == 4) {
						newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_uint, opName)).concat("; ");
					} else {
						newCode = newCode.concat(origName).concat(" = ").concat(String.format(check_gmp_ulong, opName)).concat("; ");
					}
				}
				// STEP 2: new name is associated with original version
				newName = "&" + origName;
				// STEP 3: record this variable for updating it after function call. (For handling some library calls such as scanf)
				updateInfo.put(opName, osign);
				
			} else {
				// l-value, such as array[index]
				newName = "&" + opName;
			}
			break;
		}
		case IASTUnaryExpression.op_star: {
			/*
			 * the star expression is r-value since its address is explicit
			 * moreover, its operand can be either l-value or r-value
			 * however, the operand of this expression must be of pointer type, since integer cannot be implicitly converted into pointer
			 * 
			 */
			
			assert (operand.getExpressionType() instanceof IPointerType) : "* only accepts pointer as its operand!";
			
			newName = "*(" + opName + ")";
			
			break;
			
		}
		default: {
			newCode = "";
			newName = expression.getRawSignature();
		}
		}
		
		return Pair.of(newCode, newName);
	}
	
	private Pair<String, String> scanConditionalExpression(IASTConditionalExpression expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		IASTExpression condition = expression.getLogicalConditionExpression();
		IASTExpression positive = expression.getPositiveResultExpression();
		IASTExpression negative = expression.getNegativeResultExpression();
		
		String newCode = "";
		String newName = "";
		
		Map<String, Boolean> tempMapMirror = new HashMap<>();
		copyMapFromTo(tempVarMap, tempMapMirror);
		Pair<String, String> evalCondition = scanExpressionInDFS(condition, typeAlias, tempVarMap, liftedVars);		
		String[] deltaCond = getMapDelta(tempVarMap, tempMapMirror);
		
		copyMapFromTo(tempVarMap, tempMapMirror);
		Pair<String, String> evalPos = scanExpressionInDFS(positive, typeAlias, tempVarMap, liftedVars);
		String[] deltaPos = getMapDelta(tempVarMap, tempMapMirror);
		
		// since we don't know which branch should we follow beforehand, we have to allocate different variables for positive / negative expressions
		copyMapFromTo(tempVarMap, tempMapMirror);
		Pair<String, String> evalNeg = scanExpressionInDFS(negative, typeAlias, tempVarMap, liftedVars);
		String[] deltaNeg = getMapDelta(tempVarMap, tempMapMirror);
		
		// In the following we will add codes and assign a new name for this expression
		newCode = newCode.concat(evalCondition.getFirst()).concat(evalPos.getFirst()).concat(evalNeg.getFirst());
		String condName = evalCondition.getSecond();
		String posName = evalPos.getSecond();
		String negName = evalNeg.getSecond();
		
		// since it is possible that one of pos/neg operands is highly precise and the other is not, we should make them consistent!
		// (1) if one of the operands is highly-precise, we assign a new temporary variable as the name of the whole expression;
		// (2) otherwise, we keep the form of expression.
		if(isHighPrecisionVar(posName, tempVarMap, liftedVars)) {
			// Does negative operand have high precision?
			if(!isHighPrecisionVar(negName, tempVarMap, liftedVars)) {
				// convert this expression into GMP version
				String newTempVar = getVar(tempVarMap);
				Pair<Integer, Boolean> negType = getFinalType(negative, typeAlias);
				if(negType.getSecond() == false && negType.getFirst() == 8) {
					newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, negName)).concat("; ");
				} else {
					newCode = newCode.concat(String.format(mpz_set_si, newTempVar, negName)).concat("; ");
				}
				// thus, we can demonstrate the result using only one temporary variable
				String selectExpr = "(" + condName + ") ? " + posName + " : " + newTempVar;
				newCode = newCode.concat(String.format(mpz_set, newTempVar, selectExpr)).concat("; ");
				recycleVars(deltaCond, tempVarMap);
				recycleVars(deltaPos, tempVarMap);
				recycleVars(deltaNeg, tempVarMap);
				newName = newTempVar;
			} else {
				// this one is also a GMP integer!!
				String selectExpr = "(" + condName + ") ? " + posName + " : " + negName;
				String newTempVar = getVar(tempVarMap);
				newCode = newCode.concat(String.format(mpz_set, newTempVar, selectExpr)).concat("; ");
				newName = newTempVar;
				recycleVars(deltaCond, tempVarMap);
				recycleVars(deltaPos, tempVarMap);
				recycleVars(deltaNeg, tempVarMap);
			}
		} else {
			if(isHighPrecisionVar(negName, tempVarMap, liftedVars)) {
				// convert positive operand into GMP version
				String newTempVar = getVar(tempVarMap);
				Pair<Integer, Boolean> posType = getFinalType(positive, typeAlias);
				if(posType.getSecond() == false && posType.getFirst() == 8) {
					newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, posName)).concat("; ");
				} else {
					newCode = newCode.concat(String.format(mpz_set_si, newTempVar, posName)).concat("; ");
				}
				String selectExpr = "(" + condName + ") ? " + newTempVar + " : " + negName;
				newCode = newCode.concat(String.format(mpz_set, newTempVar, selectExpr)).concat("; ");
				recycleVars(deltaCond, tempVarMap);
				recycleVars(deltaPos, tempVarMap);
				recycleVars(deltaNeg, tempVarMap);
				newName = newTempVar;
			} else {
				// we only took this case into consideration in the old version of code
				newName = "(" + condName + ") ? " + posName + " : " + negName;
			}
		}
		
		return Pair.of(newCode, newName);
	}
	
	private Pair<String, String> scanFieldReference(IASTFieldReference expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		String fieldName = expression.getFieldName().getRawSignature();
		IASTExpression owner = expression.getFieldOwner();
		
		String newCode = "";
		String newName = "";
		
		Pair<String, String> eval = scanExpressionInDFS(owner, typeAlias, tempVarMap, liftedVars);
		newCode = newCode.concat(eval.getFirst());
		
		// for now, we don't alter the structure of this expression for following reasons:
		//  (1) a field in structure has fixed type, which reflects design choice.
		//  (2) the operand of field reference (i.e. the field owner) can be either i. value; ii. pointer. The value should be of an elaborated type, and it is impossible that a integer becomes the owner of structure data.
		
		boolean isPointerDeref = expression.isPointerDereference();
		String opName = eval.getSecond();
		if(isPointerDeref) {
			newName = "(" + opName + ")->" + fieldName;
		} else {
			newName = "(" + opName + ")." + fieldName;
		}
		
		return Pair.of(newCode, newName);
	}
	
	private Pair<String, String> scanFunctionCallExpression(IASTFunctionCallExpression expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		isFuncCall = true;
		
		IASTExpression funcName = expression.getFunctionNameExpression();
		IASTInitializerClause[] funcArgs = expression.getArguments();
		int argLength = funcArgs.length;
		// in order to perform argument check before function call, we should derive the concrete type of this function
		IType type = funcName.getExpressionType();
		IFunctionType funcType;
		if(type instanceof IFunctionType) {
			funcType = (IFunctionType)type;
		} else if(type instanceof IPointerType) {
			// function name can also be a function pointer, which is of IPointerType
			IType funcPtrType = ((IPointerType)type).getType();
			if(funcPtrType instanceof IFunctionType) {
				funcType = (IFunctionType)funcPtrType;
			} else {
				// problem case
				return Pair.of("", expression.getRawSignature());
			}
		} else {
			// problem case
			return Pair.of("", expression.getRawSignature());
		}
		IType[] paramTypes = funcType.getParameterTypes();
		
		String newCode = "";
		String newName = "";
		// FIRST, process function name expression and arguments
		Pair<String, String> evalName = scanExpressionInDFS(funcName, typeAlias, tempVarMap, liftedVars);
		newCode = newCode.concat(evalName.getFirst());
		String funcNameName = evalName.getSecond();
		// this list consists of new names of argument expressions
		List<String> argNames = new ArrayList<String>();
		int index = 0;
		// we should make the size of paramTypes the same as funArgs, for vararg functions it seems impossible
		if(argLength > paramTypes.length) {
			assert (funcType.takesVarArgs()) : "Non-varadic function should have a defined number of parameters!";
			/*
			 * we need to especially handle these functions: fprintf, printf, sprintf, snprintf, vfprintf, vprintf, vsprintf, vsnprintf
			 * the position of format string:                2        1       2        (C++11)   (N/A)     (N/A)    (N/A)     (C++11)
			 * That means we need to handle only three special functions
			 * Others: without any range check (but GMP integers should be converted to ordinary integers for function call)
			 */
			if(!funcNameName.equals("fprintf") && !funcNameName.equals("sprintf") && !funcNameName.equals("printf")) {
				// ignore pre-condition check, but GMP->ordinary integer is required
				// WARNING: we should directly process function call and return in this branch!
				for(IASTInitializerClause arg : funcArgs) {
					IASTExpression argExpr = (IASTExpression)arg;
					Pair<String, String> evalArg = scanExpressionInDFS(argExpr, typeAlias, tempVarMap, liftedVars);
					newCode = newCode.concat(evalArg.getFirst());
					String thisArgName = evalArg.getSecond();
					Pair<Integer, Boolean> thisArgType = getFinalType(argExpr, typeAlias);
					int argSize = thisArgType.getFirst();
					boolean argSign = thisArgType.getSecond();
					String newArgName = "";
					if(isHighPrecisionVar(thisArgName, tempVarMap, liftedVars)) {
						// if so, convert it back to ordinary integer
						// why we don't convert to smaller integer? Because derived type is unreliable.
						if(!argSign && argSize == 8) {
							newArgName = String.format(check_gmp_ulong, thisArgName);
						} else {
							newArgName = String.format(check_gmp_slong, thisArgName);
						}
						argNames.add(newArgName);
					} else {
						// keep it
						argNames.add(thisArgName);
					}
				}
				
				// directly output this function
				newName = funcNameName + "(";
				for(int i = 0; i < argLength - 1; i++) {
					newName = newName.concat(argNames.get(i).concat(", "));
				}
				if(argLength > 0) {
					newName = newName.concat(argNames.get(argLength - 1));
				}
				newName = newName.concat(")");
				
				// return early
				return Pair.of(newCode, newName);
				
			} else {
				IASTExpression fmtStringExpr;
				if(funcNameName.equals("fprintf") || funcNameName.equals("sprintf")) {
					fmtStringExpr = (IASTExpression)funcArgs[1];
				} else {
					fmtStringExpr = (IASTExpression)funcArgs[0];
				}
				Pair<String, String> evalFmtString = scanExpressionInDFS(fmtStringExpr, typeAlias, tempVarMap, liftedVars);
				String fmtLiteral = evalFmtString.getSecond();
				CFormatStringParseResult[] results = parseCFormatString(fmtLiteral);
				
				// by parse result we add type constraints on remaining parameters
				/*
				 * How to construct a type object?
				 * IType is interface, we need to invoke the constructor of a concrete class
				 * (1) Integers --> CBasicType
				 * (2) Pointers --> CPointerType (since it is of IPointerType, )
				 * 
				 */
				List<IType> extendedParamType = new ArrayList<>();
				for(int i = 0; i < paramTypes.length; i++) {
					extendedParamType.add(paramTypes[i]);
				}
				// Then extend this list according to parse result
				for(CFormatStringParseResult result : results) {
					// FIRST, check if user needs to specify width
					if(result.isCustomWidth()) {
						// unsigned int
						CBasicType newType = new CBasicType(Kind.eInt, IBasicType.IS_UNSIGNED);
						extendedParamType.add(newType);
					}
					if(result.isCustomPrecision()) {
						// unsigned int, as the same
						CBasicType newType = new CBasicType(Kind.eInt, IBasicType.IS_UNSIGNED);
						extendedParamType.add(newType);
					}
					// But most importantly, we will write out type information for the specifier
					int signInfo = result.getSigned();
					int lengthInfo = result.getLength();
					if(signInfo == CFormatStringParseResult.s_ni) {
						// we don't try to intervene this type...pointer type can help us keep this type 
						extendedParamType.add(CPointerType.VOID_POINTER);
					} else {
						if(signInfo == CFormatStringParseResult.s_sizet) {
							CBasicType thisType = new CBasicType(Kind.eInt, IBasicType.IS_UNSIGNED | IBasicType.IS_LONG);
							extendedParamType.add(thisType);
						} else {
							// discuss sign and length by routine
							int tModifier = 0;
							if(signInfo == CFormatStringParseResult.s_signed) {
								tModifier = IBasicType.IS_SIGNED;
							} else {
								tModifier = IBasicType.IS_UNSIGNED;
							}
							
							if(lengthInfo == CFormatStringParseResult.l_char) {
								CBasicType thisType = new CBasicType(Kind.eChar, tModifier);
								extendedParamType.add(thisType);
							} else if(lengthInfo == CFormatStringParseResult.l_short) {
								CBasicType thisType = new CBasicType(Kind.eInt, tModifier | IBasicType.IS_SHORT);
								extendedParamType.add(thisType);
							} else if(lengthInfo == CFormatStringParseResult.l_int) {
								CBasicType thisType = new CBasicType(Kind.eInt, tModifier);
								extendedParamType.add(thisType);
							} else if(lengthInfo == CFormatStringParseResult.l_long) {
								CBasicType thisType = new CBasicType(Kind.eInt, tModifier | IBasicType.IS_LONG);
								extendedParamType.add(thisType);
							} else {
								// LONG LONG
								CBasicType thisType = new CBasicType(Kind.eInt, tModifier | IBasicType.IS_LONG_LONG);
								extendedParamType.add(thisType);
							}
						}
					}
				}
				// SECOND, update type list, and check if the number of arguments is consistent with the format string
				int paramSize = extendedParamType.size();
				for(int i = paramSize; i < funcArgs.length; i++) {
					extendedParamType.add(CPointerType.VOID_POINTER);
				}
				
				paramTypes = extendedParamType.toArray(new IType[extendedParamType.size()]);
			}
		}
		
		// if we reach here, there are 2 cases:
		// (1) the function takes variable arguments, but the types of missing arguments can be derived from format string;
		// (2) the function takes a fixed number of arguments, it is a normal function call
		
		for(IASTInitializerClause arg : funcArgs) {
			IASTExpression argExpr = (IASTExpression)arg;
			Pair<String, String> evalArg = scanExpressionInDFS(argExpr, typeAlias, tempVarMap, liftedVars);
			newCode = newCode.concat(evalArg.getFirst());
			// How about this case? This parameter is a SHORT integer but we pass a LONG integer as argument, then overflow probably occurs as the result
			// Therefore, we should check fitting of arguments before attaching them to the parameters
			
			// FIRST, check this argument
			String thisArgName = evalArg.getSecond();
			String newArgName = "";
			Pair<Integer, Boolean> thisParamType = getFinalType(paramTypes[index], typeAlias);
			int paramLength = thisParamType.getFirst();
			boolean paramSigned = thisParamType.getSecond();
			if(isHighPrecisionVar(thisArgName, tempVarMap, liftedVars)) {
				// this is a temporary variable or a local variable
				// ** In general, such value is passed to the parameter of numerical type, but exception could exist
				if(paramSigned) {
					if(paramLength == 1) {
						newArgName = String.format(check_gmp_schar, thisArgName);
					} else if(paramLength == 2) {
						newArgName = String.format(check_gmp_sshort, thisArgName);
					} else if(paramLength == 4) {
						newArgName = String.format(check_gmp_int, thisArgName);
					} else {
						newArgName = String.format(check_gmp_slong, thisArgName);
					}
				} else {
					if(paramLength == 1) {
						newArgName = String.format(check_gmp_uchar, thisArgName);
					} else if(paramLength == 2) {
						newArgName = String.format(check_gmp_ushort, thisArgName);
					} else if(paramLength == 4) {
						newArgName = String.format(check_gmp_uint, thisArgName);
					} else {
						newArgName = String.format(check_gmp_ulong, thisArgName);
					}
				}
				argNames.add(newArgName);
			} else {
				// this is a ordinary expression which could contain a temporary variable
				Pair<Integer, Boolean> thisArgType = getFinalType(argExpr, typeAlias);
				if(paramLength == -1) {
					// that means, this parameter only accept non-numerical value (since a numerical value cannot be converted into numerical value implicitly)
					newArgName = thisArgName;
				}
				else {
					String thisSign = thisArgType.getSecond() ? "1" : "0";
					if(paramSigned) {
						if(paramLength == 1) {
							newArgName = String.format(check_schar, thisArgName, thisSign);
						} else if(paramLength == 2) {
							newArgName = String.format(check_sshort, thisArgName, thisSign);
						} else if(paramLength == 4) {
							newArgName = String.format(check_int, thisArgName, thisSign);
						} else {
							newArgName = String.format(check_slong, thisArgName, thisSign);
						}
					} else {
						if(paramLength == 1) {
							newArgName = String.format(check_uchar, thisArgName, thisSign);
						} else if(paramLength == 2) {
							newArgName = String.format(check_ushort, thisArgName, thisSign);
						} else if(paramLength == 4) {
							newArgName = String.format(check_uint, thisArgName, thisSign);
						} else {
							newArgName = String.format(check_ulong, thisArgName, thisSign);
						}
					}
				}
				argNames.add(newArgName);
			}
			index++;
		}
		
		// do you find that we didn't introduce any new temporary variables here? It is a very important feature for our lemma.
		newName = funcNameName + "(";
		for(int i = 0; i < argLength - 1; i++) {
			newName = newName.concat(argNames.get(i)).concat(", ");
		}
		if(argLength > 0) {
			newName = newName.concat(argNames.get(argLength - 1));
		}
		newName = newName.concat(")");
		
		return Pair.of(newCode, newName);
	}
	
	private Pair<String, String> scanIdExpression(IASTIdExpression expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		/*
		 * If this variable is the global/local integer variable, then it appears in liftedVars set. We should return the new variable name with GMP prefix.
		 * Otherwise, we keep this variable.
		 * 
		 */
		String newName = "";
		String origName = expression.getName().getRawSignature();
		if(liftedVars.contains(origName)) {
			newName = prefix + origName;
		} else {
			newName = origName;
		}
		
		// try to find out whether this id is a pointer
		String derefVal = getDerefValue(origName);
		if(derefVal != null && isHighPrecisionVar(derefVal, tempVarMap, liftedVars)) {
			boolean sign = signTable.get(derefVal);
			updateInfo.put(derefVal, sign);
		}
		
		return Pair.of("", newName);
	}
	
	private String getDerefValue(String originalName) {
		if(!pointerInfo.containsKey(originalName)) {
			return null;
		} else {
			String prev = "";
			String curr = originalName;
			while(curr != null) {
				prev = curr;
				curr = pointerInfo.get(curr);
			}
			return prev;
		}
	}
	
	private Pair<String, String> scanLiteralExpression(IASTLiteralExpression expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		/*
		 * integer literal are always interpreted as "int" type despite of its actual value. 
		 * thus, integer literal is always signed
		 * 
		 * in general, this literal cannot be used directly as operand, instead it will be wrapped for further processing.
		 * check_u(s)* method takes a signed/unsigned long integer and sign information, thus raw literal will be regarded as a signed long integer by default
		 * If this literal exceeds the range of SIGNED_LONG, we should aggressively convert this number into GMP integer
		 * 
		 */
		String newCode = "";
		String newName = "";
		int literalKind = expression.getKind();
		String litString = String.valueOf(expression.getValue()); // this is safe, and work for MACROs
		if(literalKind == IASTLiteralExpression.lk_integer_constant) {
			litString = toIntLiteral(litString); // remove tail characters such as 'l' and 'u'
			BigInteger value;
			// bin, oct, dec or hex?
			if(litString.toLowerCase().startsWith("0x")) {
				litString = litString.substring(2);
				value = new BigInteger(litString, 16);
			} else if(litString.startsWith("0") && litString.length() > 1) {
				litString = litString.substring(1);
				value = new BigInteger(litString, 8);
			} else {
				value = new BigInteger(litString);
			}
			BigInteger longMin = BigInteger.valueOf(Long.MIN_VALUE);
			BigInteger longMax = BigInteger.valueOf(Long.MAX_VALUE);
			if(value.compareTo(longMin) == -1 || value.compareTo(longMax) == 1) {
				// convert this integer to GMP integer
				String newTempVar = getVar(tempVarMap);
				newCode = newCode.concat(String.format(mpz_set_str, newTempVar, "\"" + litString + "\"", "0")).concat("; ");
				newName = newTempVar;
			} else {
				newName = value.toString();
			}
		} else {
			// boolean, char: numerical value, but they are definitely inside the range of SIGNED_LONG
			// null pointer, string literal, float: they are not considered in this application
			newName = litString;
		}
		
		return Pair.of(newCode, newName);
	}
	
	private Pair<String, String> scanCastExpression(IASTCastExpression expression, Map<String, String> typeAlias, Map<String, Boolean> tempVarMap, Set<String> liftedVars) {
		/*
		 * Type can be simple numerical type (NT) or other types (IT)
		 * Value can be numerical value (nv) or other values (iv)
		 * 
		 * Then, we should discuss 4 different cases:
		 * (1) (NT) nv : in this case, we just wrap nv using a GMP integer
		 * (2) (IT) nv : in this case, we convert nv into ULONG first (i.e. a machine number)
		 * (3) (NT) iv : in this case, we just wrap the conversion result using a GMP integer
		 * (4) (IT) iv : we do nothing
		 */
		String newCode = "";
		String newName = "";
		IASTExpression operand = expression.getOperand();
		IASTTypeId typeId = expression.getTypeId();
		Pair<Integer, Boolean> operandType = getFinalType(operand, typeAlias);
		boolean operandIsNum = (operandType.getFirst() != -1);
		IASTDeclSpecifier typeSpec = typeId.getDeclSpecifier();
		IASTDeclarator absDecl = typeId.getAbstractDeclarator();
		boolean typeIdIsNum = false;
		// FIRST, check declaration specifier
		if(typeSpec instanceof IASTSimpleDeclSpecifier) {
			typeIdIsNum = !isNotIntegerSpecifier((IASTSimpleDeclSpecifier)typeSpec);
		} else if(typeSpec instanceof IASTEnumerationSpecifier) {
			typeIdIsNum = true;
		} else if(typeSpec instanceof IASTNamedTypeSpecifier) {
			// we need to search for alias of this type
			typeIdIsNum = isAliasOfInteger((IASTNamedTypeSpecifier)typeSpec, typeAlias);
		} else {
			typeIdIsNum = false;
		}
		// SECOND, check abstract declarator (typically without initializer clause and name)
		String absDeclStr = absDecl.getRawSignature();
		if(!absDeclStr.isEmpty()) {
			typeIdIsNum = false;
		}
		
		Pair<String, String> evaloperand = scanExpressionInDFS(operand, typeAlias, tempVarMap, liftedVars);
		newCode = newCode.concat(evaloperand.getFirst());
		String opName = evaloperand.getSecond();
		
		// THIRD, discuss 4 cases above
		if(typeIdIsNum) {
			// Numerical type, then the value of this expression should be numerical
			if(operandIsNum) {
				// case 1
				if(isHighPrecisionVar(opName, tempVarMap, liftedVars)) {
					// do nothing
					newName = opName;
				} else {
					// wrap it using GMP integer
					String newTempVar = getVar(tempVarMap);
					if(operandType.getSecond() == false && operandType.getFirst() == 8) {
						newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, opName)).concat("; ");
					} else {
						newCode = newCode.concat(String.format(mpz_set_si, newTempVar, opName)).concat("; ");
					}
					newName = newTempVar;
				}
			} else {
				// case 3
				String newTempVar = getVar(tempVarMap);
				newCode = newCode.concat(String.format(mpz_set_ui, newTempVar, opName)).concat("; ");
				newName = newTempVar;
			}
		} else {
			if(operandIsNum) {
				// case 2
				String sign = operandType.getSecond() ? "1" : "0";
				String newOprd = String.format(check_ulong, opName, sign);
				newName = "(" + typeId.getRawSignature() + ")" + newOprd;
			} else {
				// case 4
				newName = "(" + typeId.getRawSignature() + ")" + opName;
			}
		}
		
		return Pair.of(newCode, newName);
		
	}
	
	private String handleShiftOperationAssign(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name, 
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		String addCode = "";
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		boolean leftHP = false;
		String newTempVar, rightOp;
		
		newTempVar = getVar(tempVarMap);
		if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
			leftHP = true;
		} else {
			leftHP = false;
			if((ltype.getSecond() == false && ltype.getFirst() == 8) || ltype.getFirst() == -1) {
				addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op1Name));
			}
		}
		// right operand should be a unsigned long
		if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
			rightOp = String.format(check_gmp_ulong, op2Name);
		} else {
			String rsign = rtype.getSecond() ? "1" : "0";
			rightOp = String.format(check_ulong, op2Name, rsign);
		}
		
		if(leftHP) {
			addCode = addCode.concat(generateBinaryOperation(op1Name, op1Name, rightOp, optrString)).concat("; ");
		} else {
			addCode = addCode.concat(generateBinaryOperation(newTempVar, newTempVar, rightOp, optrString)).concat("; ");
			// newTempVar should be cast to the left operand op1Name
			boolean lsign = ltype.getSecond();
			int llength = ltype.getFirst();
			if(lsign) {
				if(llength == 1) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_schar, newTempVar)).concat("; ");
				} else if(llength == 2) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_sshort, newTempVar)).concat("; ");
				} else if(llength == 4) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_int, newTempVar)).concat("; ");
				} else {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_slong, newTempVar)).concat("; ");
				}
			} else {
				if(llength == 1) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_uchar, newTempVar)).concat("; ");
				} else if(llength == 2) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_ushort, newTempVar)).concat("; ");
				} else if(llength == 4) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_uint, newTempVar)).concat("; ");
				} else {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_ulong, newTempVar)).concat("; ");
				}
			}
		}
		recycleVar(newTempVar, tempVarMap);
		
		return addCode;
	}
	
	private Pair<String, String> handleShiftOperation(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name,
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		// operand2 must be a non-negative value, otherwise we cause an undefined behavior.
		String addCode = "";
		String addName = "";
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		String rightOp;
		String newTempVar = "";
		
		// FIRST, left operand should be converted to GMP integer first
		if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
			newTempVar = getVar(tempVarMap);
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				addCode = addCode.concat(generateBinaryOperation(newTempVar, op1Name, op2Name, optrString)).concat("; ");
				addName = newTempVar;
			} else {
				if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op2Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, newTempVar, op2Name)).concat("; ");
				}
				addCode = addCode.concat(generateBinaryOperation(newTempVar, op1Name, newTempVar, optrString)).concat("; ");
				addName = newTempVar;
			}
		} else {
			newTempVar = getVar(tempVarMap);
			if((ltype.getSecond() == false && ltype.getFirst() == 8) || ltype.getFirst() == -1) {
				addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op1Name)).concat("; ");
			} else {
				addCode = addCode.concat(String.format(mpz_set_si, newTempVar, op1Name)).concat("; ");
			}
			// SECOND, right operand should be casted to unsigned long with great care
			String rsign = rtype.getSecond() ? "1" : "0";
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				rightOp = String.format(check_gmp_ulong, op2Name);
			} else {
				rightOp = String.format(check_ulong, op2Name, rsign);
			}
			addCode = addCode.concat(generateBinaryOperation(newTempVar, newTempVar, rightOp, optrString)).concat("; ");
			addName = newTempVar;
		}

		return Pair.of(addCode, addName);
	}
	
	private String handleBinaryOperationAssign(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name, 
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		// The result should be assigned to GMP integer or casted as a machine integer
		String addCode = "";
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		String leftName, rightName;
		boolean lsign = ltype.getSecond();
		int llength = ltype.getFirst();
		if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
			// Then the final result can be directly assigned to GMP integer
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				addCode = addCode.concat(generateBinaryOperation(op1Name, op1Name, op2Name, optrString)).concat("; ");
			} else {
				rightName = getVar(tempVarMap);
				if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, rightName, op2Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, rightName, op2Name)).concat("; ");
				}
				addCode = addCode.concat(generateBinaryOperation(op1Name, op1Name, rightName, optrString)).concat("; ");
				recycleVar(rightName, tempVarMap);
			}
		} else {
			// Then we have to fit the GMP integer to a machine integer
			// first, store the first operand into a GMP integer
			leftName = getVar(tempVarMap);
			if((ltype.getSecond() == false && ltype.getFirst() == 8) || ltype.getFirst() == -1) {
				addCode = addCode.concat(String.format(mpz_set_ui, leftName, op1Name)).concat("; ");
			} else {
				addCode = addCode.concat(String.format(mpz_set_si, leftName, op1Name)).concat("; ");
			}
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				addCode = addCode.concat(generateBinaryOperation(leftName, leftName, op2Name, optrString)).concat("; ");
			} else {
				rightName = getVar(tempVarMap);
				if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, rightName, op2Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, rightName, op2Name)).concat("; ");
				}
				addCode = addCode.concat(generateBinaryOperation(leftName, leftName, rightName, optrString)).concat("; ");
				recycleVar(rightName, tempVarMap);
			}
			
			// now leftName refers to the GMP version of result, now casting it to corresponding machine integer
			if(lsign) {
				if(llength == 1) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_schar, leftName)).concat("; ");
				} else if(llength == 2) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_sshort, leftName)).concat("; ");
				} else if(llength == 4) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_int, leftName)).concat("; ");
				} else {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_slong, leftName)).concat("; ");
				}
			} else {
				if(llength == 1) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_uchar, leftName)).concat("; ");
				} else if(llength == 2) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_ushort, leftName)).concat("; ");
				} else if(llength == 4) {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_uint, leftName)).concat("; ");
				} else {
					addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_ulong, leftName)).concat("; ");
				}
			}
			recycleVar(leftName, tempVarMap);
		}
		
		return addCode;
	}
	
	private Pair<String, String> handleBinaryOperation(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name,
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		// STEP1: convert both values to GMP values; STEP2: calculating results using GMP function; STEP3: Name: A temp var
		// NOTE: you should maintain recycling for yourself!
		String addCode = "";
		String addName = "";
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		String newTempVar, newTempVar2;
		if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
			newTempVar = getVar(tempVarMap);
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				// modifying an operand directly is incorrect if this operand is a local variable.
				addCode = addCode.concat(generateBinaryOperation(newTempVar, op1Name, op2Name, optrString)).concat("; ");
				addName = newTempVar;
			} else {
				if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op2Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, newTempVar, op2Name)).concat("; ");
				}
				addCode = addCode.concat(generateBinaryOperation(newTempVar, op1Name, newTempVar, optrString)).concat("; ");
				addName = newTempVar;
			}
		} else {
			newTempVar = getVar(tempVarMap);
			if((ltype.getSecond() == false && ltype.getFirst() == 8) || ltype.getFirst() == -1) {
				addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op1Name)).concat("; ");
			} else {
				addCode = addCode.concat(String.format(mpz_set_si, newTempVar, op1Name)).concat("; ");
			}
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				addCode = addCode.concat(generateBinaryOperation(newTempVar, newTempVar, op2Name, optrString)).concat("; ");
				addName = newTempVar;
			} else {
				newTempVar2 = getVar(tempVarMap);
				if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, newTempVar2, op2Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, newTempVar2, op2Name)).concat("; ");
				}
				addCode = addCode.concat(generateBinaryOperation(newTempVar, newTempVar, newTempVar2, optrString)).concat("; ");
				addName = newTempVar;
				recycleVar(newTempVar2, tempVarMap);
			}
		}
		
		return Pair.of(addCode, addName);
	}
	
	private String generateBinaryOperation(String opr, String op1, String op2, String optr) {
		if(optr.equals("*")) {
			return String.format(mpz_mul, opr, op1, op2);
		} else if(optr.equals("/")) {
			return String.format(mpz_tdiv_q, opr, op1, op2);
		} else if(optr.equals("%")) {
			return String.format(mpz_tdiv_r, opr, op1, op2);
		} else if(optr.equals("<<")) {
			return String.format(mpz_mul_2exp, opr, op1, op2);
		} else {
			// right-shift
			return String.format(mpz_fdiv_q_2exp, opr, op1, op2);
		}
	}
	
	private String handlePlusAndMinusAssign(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name, 
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		// assignment, its value should be always "1"
		String addCode = "";
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		boolean lsign = ltype.getSecond();
		boolean rsign = rtype.getSecond();
		boolean isPlus = optrString.equals("+");
		if(ltype.getFirst() == -1 && (operand1.getExpressionType() instanceof IPointerType)) {
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				// GMP integer, let us convert it into SIGNED_LONG to enable negative pointer arithmetic
				if(rsign == false && rtype.getFirst() == 8) {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_ul, op1Name, String.format(check_gmp_ulong, op2Name))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_ul, op1Name, String.format(check_gmp_ulong, op2Name))).concat("; ");
					}
					addCode = addCode.concat(op1Name + " = " + op1Name + " " + optrString + " " + String.format(check_gmp_ulong, op2Name) + "; ");
				} else {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_sl, op1Name, String.format(check_gmp_slong, op2Name))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_sl, op1Name, String.format(check_gmp_slong, op2Name))).concat("; ");
					}
					addCode = addCode.concat(op1Name + " = " + op1Name + " " + optrString + " " + String.format(check_gmp_slong, op2Name) + "; ");
				}
			} else {
				if(rsign) {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_sl, op1Name, String.format(check_slong, op2Name, "1"))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_sl, op1Name, String.format(check_slong, op2Name, "1"))).concat("; ");
					}
					addCode = addCode.concat(op1Name + " = " + op1Name + " " + optrString + " " + String.format(check_slong, op2Name, "1") + "; ");
				} else {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_ul, op1Name, String.format(check_ulong, op2Name, "0"))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_ul, op1Name, String.format(check_ulong, op2Name, "0"))).concat("; ");
					}
					addCode = addCode.concat(op1Name + " = " + op1Name + " " + optrString + " " + String.format(check_ulong, op2Name, "0") + "; ");
				}
			}
		} else {
			// both sides are numerical values...
			String leftName = "", rightName = "";
			boolean leftHP = false;
			if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
				leftName = op1Name;
				leftHP = true;
			} else {
				leftName = getVar(tempVarMap);
				if((lsign == false && ltype.getFirst() == 8) || ltype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, leftName, op1Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, leftName, op1Name)).concat("; ");
				}
				leftHP = false;
			}
			
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				rightName = op2Name;
			} else {
				rightName = getVar(tempVarMap);
				if((rsign == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, rightName, op2Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, rightName, op2Name)).concat("; ");
				}
			}
			
			// NOTE: since the left side is a left-value, it is impossible to be a temporary variable
			// Temporary variables are used to represent intermediate results, these results are undoubtfully r-value
			if(isPlus) {
				addCode = addCode.concat(String.format(mpz_add, leftName, leftName, rightName)).concat("; ");
			} else {
				addCode = addCode.concat(String.format(mpz_sub, leftName, leftName, rightName)).concat("; ");
			}
			
			if(!leftHP) {
				// if leftHP is true, then we have assigned result to left operand, nothing is to do
				int llength = ltype.getFirst();
				if(!lsign) {
					if(llength == 1) {
						addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_uchar, leftName)).concat("; ");
					} else if(llength == 2) {
						addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_ushort, leftName)).concat("; ");
					} else if(llength == 4) {
						addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_uint, leftName)).concat("; ");
					} else {
						addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_ulong, leftName)).concat("; ");
					}
				} else {
					if(llength == 1) {
						addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_schar, leftName)).concat("; ");
					} else if(llength == 2) {
						addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_sshort, leftName)).concat("; ");
					} else if(llength == 4) {
						addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_int, leftName)).concat("; ");
					} else {
						addCode = addCode.concat(op1Name).concat(" = ").concat(String.format(check_gmp_slong, leftName)).concat("; ");
					}
				}
			}
			
			// recycle these temporary variables
			recycleVar(leftName, tempVarMap);
			recycleVar(rightName, tempVarMap);
		}
		
		return addCode;
	}
	
	private Pair<String, String> handlePlusAndMinus(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name,
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		String addCode = "";
		String addName = "";
		// Unlike other handling functions, we will first check and recognize the type of operands. For the "P+N" case, we should keep the pointer type; for other cases we convert pointer to integer.
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		boolean lsign = ltype.getSecond();
		boolean rsign = rtype.getSecond();
		boolean isPlus = optrString.equals("+"); // false if the operator is minus
		// Pointer case, i.e. P+N
		if(ltype.getFirst() == -1 && (operand1.getExpressionType() instanceof IPointerType) && rtype.getFirst() != -1) {
			// check whether right operand is a temporary variable
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				// GMP integer, let us convert it into SIGNED_LONG to enable negative pointer arithmetic
				// we should not only assert a constraint on offset, but also on the whole value!
				if(rsign == false && rtype.getFirst() == 8) {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_ul, op1Name, String.format(check_gmp_ulong, op2Name))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_ul, op1Name, String.format(check_gmp_ulong, op2Name))).concat("; ");
					}
					addName = op1Name + " " + optrString + " " + String.format(check_gmp_ulong, op2Name);
				} else {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_sl, op1Name, String.format(check_gmp_slong, op2Name))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_sl, op1Name, String.format(check_gmp_slong, op2Name))).concat("; ");
					}
					addName = op1Name + " " + optrString + " " + String.format(check_gmp_slong, op2Name);
				}
			} else {
				if(rsign) {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_sl, op1Name, String.format(check_slong, op2Name, "1"))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_sl, op1Name, String.format(check_slong, op2Name, "1"))).concat("; ");
					}
					addName = op1Name + " " + optrString + " " + String.format(check_slong, op2Name, "1");
				} else {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_ul, op1Name, String.format(check_ulong, op2Name, "0"))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_ul, op1Name, String.format(check_ulong, op2Name, "0"))).concat("; ");
					}
					addName = op1Name + " " + optrString + " " + String.format(check_ulong, op2Name, "0");
				}
			}
		} else if(rtype.getFirst() == -1 && (operand2.getExpressionType() instanceof IPointerType) && ltype.getFirst() != -1) {
			// THE SAME!
			if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
				// GMP integer, let us convert it into SIGNED_LONG to enable negative pointer arithmetic
				if(lsign == false && ltype.getFirst() == 8) {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_ul, op2Name, String.format(check_gmp_ulong, op1Name))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_ul, op2Name, String.format(check_gmp_ulong, op1Name))).concat("; ");
					}
					addName = op2Name + " " + optrString + " " + String.format(check_gmp_ulong, op1Name);
				} else {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_sl, op2Name, String.format(check_gmp_slong, op1Name))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_sl, op2Name, String.format(check_gmp_slong, op1Name))).concat("; ");
					}
					addName = op2Name + " " + optrString + " " + String.format(check_gmp_slong, op1Name);
				}
			} else {
				if(lsign) {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_sl, op2Name, String.format(check_slong, op1Name, "1"))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_sl, op2Name, String.format(check_slong, op1Name, "1"))).concat("; ");
					}
					addName = op2Name + " " + optrString + " " + String.format(check_slong, op1Name, "1");
				} else {
					if(isPlus) {
						addCode = addCode.concat(String.format(check_pointer_plus_ul, op2Name, String.format(check_ulong, op1Name, "0"))).concat("; ");
					} else {
						addCode = addCode.concat(String.format(check_pointer_minus_ul, op2Name, String.format(check_ulong, op1Name, "0"))).concat("; ");
					}
					addName = op2Name + " " + optrString + " " + String.format(check_ulong, op1Name, "0");
				}
			}
		} else {
			// otherwise, both operands are treated as numerical values
			String newTempVar = getVar(tempVarMap);
			if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
				if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
					// add/minus two operands and store the result in newTempVar
					if(isPlus) {
						addCode = addCode.concat(String.format(mpz_add, newTempVar, op1Name, op2Name)).concat("; ");
					} else {
						// has to be "-"
						addCode = addCode.concat(String.format(mpz_sub, newTempVar, op1Name, op2Name)).concat("; ");
					}
					addName = newTempVar;
				} else {
					// sorry, the 2nd operand is not GMP integer
					if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
						addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op2Name)).concat("; ");
					} else {
						addCode = addCode.concat(String.format(mpz_set_si, newTempVar, op2Name)).concat("; ");
					}
					// add or minus them
					if(isPlus) {
						addCode = addCode.concat(String.format(mpz_add, newTempVar, op1Name, newTempVar)).concat("; ");
					} else {
						addCode = addCode.concat(String.format(mpz_sub, newTempVar, op1Name, newTempVar)).concat("; ");
					}
					addName = newTempVar;
				}
			} else {
				// 1st operand is not GMP integer
				if((ltype.getSecond() == false && ltype.getFirst() == 8) || ltype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op1Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, newTempVar, op1Name)).concat("; ");
				}
				if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
					// 2nd operand is a GMP integer
					if(isPlus) {
						addCode = addCode.concat(String.format(mpz_add, newTempVar, newTempVar, op2Name)).concat("; ");
					} else {
						addCode = addCode.concat(String.format(mpz_sub, newTempVar, newTempVar, op2Name)).concat("; ");
					}
				} else {
					// 2nd operand is a normal integer
					String newTempVar2 = getVar(tempVarMap);
					if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
						addCode = addCode.concat(String.format(mpz_set_ui, newTempVar2, op2Name)).concat("; ");
					} else {
						addCode = addCode.concat(String.format(mpz_set_si, newTempVar2, op2Name)).concat("; ");
					}
					if(isPlus) {
						addCode = addCode.concat(String.format(mpz_add, newTempVar, newTempVar, newTempVar2)).concat("; ");
					} else {
						addCode = addCode.concat(String.format(mpz_sub, newTempVar, newTempVar, newTempVar2)).concat("; ");
					}
					// recycle wrong variable
					recycleVar(newTempVar2, tempVarMap);
				}
				addName = newTempVar;
			}
		}
		return Pair.of(addCode, addName);
	}
	
	private Pair<String, String> handleLogicalArithmetic(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name,
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		// TODO: maybe we can do better if we improve our heuristic
		String addCode = "";
		String addName = "";
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		String newTempVar, newTempVar2;
		newTempVar = getVar(tempVarMap);
		newTempVar2 = getVar(tempVarMap);
		if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
			addCode.concat(String.format(mpz_set, newTempVar, op1Name)).concat("; ");
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				addCode.concat(String.format(mpz_set, newTempVar2, op2Name)).concat("; ");
			} else {
				if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
					addCode.concat(String.format(mpz_set_ui, newTempVar2, op2Name)).concat("; ");
				} else {
					addCode.concat(String.format(mpz_set_si, newTempVar2, op2Name)).concat("; ");
				}
			}
		} else {
			if((ltype.getSecond() == false && ltype.getFirst() == 8) || ltype.getFirst() == -1) {
				addCode.concat(String.format(mpz_set_ui, newTempVar, op1Name)).concat("; ");
			} else {
				addCode.concat(String.format(mpz_set_si, newTempVar, op1Name)).concat("; ");
			}
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				addCode.concat(String.format(mpz_set, newTempVar2, op2Name));
			} else {
				if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
					addCode.concat(String.format(mpz_set_ui, newTempVar2, op2Name)).concat("; ");
				} else {
					addCode.concat(String.format(mpz_set_si, newTempVar2, op2Name)).concat("; ");
				}
			}
		}
		addName = "(" + String.format(mpz_cmp_si, newTempVar, "0") + " == 0 " + optrString + " " + String.format(mpz_cmp_si, newTempVar2, "0") + " == 0) ? 0 : 1";
		
		return Pair.of(addCode, addName);
	}
	
	// RETURN: additional code and new name
	private Pair<String, String> handleComparisonPredicate(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name,
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		// We can handle other predicate operations similarly
		// ATTENTION! operand can be numerical numbers or POINTERS! In gcc, pointers can be implicitly casted to a number of long type
		// address the issue when two operands are not numerical values.
		String addCode = "";
		String addName = "";
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		String newTempVar;
		if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				newTempVar = getVar(tempVarMap);
				addCode = addCode.concat(String.format(mpz_set_si, newTempVar, String.format(mpz_cmp, op1Name, op2Name))).concat("; ");
				addName = String.format(mpz_cmp_si, newTempVar, "0") + " " + optrString + " 0 ? 1 : 0";
			} else {
				// operand2 is a normal value, we should convert it into GMP integer first
				newTempVar = getVar(tempVarMap);		
				// unsigned long AND pointer type!!
				if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op2Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, newTempVar, op2Name)).concat("; ");
				}
				// OK! we can compare them!
				addCode = addCode.concat(String.format(mpz_set_si, newTempVar, String.format(mpz_cmp, op1Name, newTempVar))).concat("; ");
				addName = String.format(mpz_cmp_si, newTempVar, "0") + " " + optrString + " 0 ? 1 : 0";
				
			}
		} else {
			if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
				// operand 2 is a GMP value but operand 1 is not.
				newTempVar = getVar(tempVarMap);
				// if someone takes a pointer as an operand...
				if((ltype.getSecond() == false && ltype.getFirst() == 8) || ltype.getFirst() == -1) {
					addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op1Name)).concat("; ");
				} else {
					addCode = addCode.concat(String.format(mpz_set_si, newTempVar, op1Name)).concat("; ");
				}
				// OK! we can compare them!
				addCode = addCode.concat(String.format(mpz_set_si, newTempVar, String.format(mpz_cmp, newTempVar, op2Name))).concat("; ");
				addName = String.format(mpz_cmp_si, newTempVar, "0") + " " + optrString + " 0 ? 1 : 0";
			} else {
				// Ooops! Two operands are neither GMP integers.
				// *** for instance, two operands are pointers
				String newTempVar2;
				newTempVar = getVar(tempVarMap);
				newTempVar2 = getVar(tempVarMap);
				if(ltype.getFirst() == -1 && rtype.getFirst() == -1) {
					addName = op1Name + " " + optrString + " " + op2Name;
				} else {
					if((ltype.getSecond() == false && ltype.getFirst() == 8) || ltype.getFirst() == -1) {
						addCode = addCode.concat(String.format(mpz_set_ui, newTempVar, op1Name)).concat("; ");
					} else {
						addCode = addCode.concat(String.format(mpz_set_si, newTempVar, op1Name)).concat("; ");
					}
					if((rtype.getSecond() == false && rtype.getFirst() == 8) || rtype.getFirst() == -1) {
						addCode = addCode.concat(String.format(mpz_set_ui, newTempVar2, op2Name)).concat("; ");
					} else {
						addCode = addCode.concat(String.format(mpz_set_si, newTempVar2, op2Name)).concat("; ");
					}
					recycleVar(newTempVar2, tempVarMap);
					addCode = addCode.concat(String.format(mpz_set_si, newTempVar, String.format(mpz_cmp, newTempVar, newTempVar2))).concat("; ");
					addName = String.format(mpz_cmp_si, newTempVar, "0") + " " + optrString + " 0 ? 1 : 0";
				}
			}
		}
		return Pair.of(addCode, addName);
	}
	
	// bit operation does not produce new codes or new intermediate variables
	// RETURN: new name of this expression
	private String handleBitOperation(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name, 
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		int thisLength = Math.max(ltype.getFirst(), rtype.getFirst());
		String lsign = ltype.getSecond() == true ? "1" : "0";
		String rsign = rtype.getSecond() == true ? "1" : "0";
		String leftOp = ""; 
		String rightOp = "";
		
		if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
			if(thisLength == 1) {
				leftOp = String.format(check_gmp_uchar, op1Name);
			} else if(thisLength == 2) {
				leftOp = String.format(check_gmp_ushort, op1Name);
			} else if(thisLength == 4) {
				leftOp = String.format(check_gmp_uint, op1Name);
			} else {
				leftOp = String.format(check_gmp_ulong, op1Name);
			}
		} else {
			if(thisLength == 1) {
				leftOp = String.format(check_uchar, op1Name, lsign);
			} else if(thisLength == 2) {
				leftOp = String.format(check_ushort, op1Name, lsign);
			} else if(thisLength == 4) {
				leftOp = String.format(check_uint, op1Name, lsign);
			} else if(thisLength == 8) {
				leftOp = String.format(check_ulong, op1Name, lsign);
			}
		}
		
		if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
			if(thisLength == 1) {
				rightOp = String.format(check_gmp_uchar, op2Name);
			} else if(thisLength == 2) {
				rightOp = String.format(check_gmp_ushort, op2Name);
			} else if(thisLength == 4) {
				rightOp = String.format(check_gmp_uint, op2Name);
			} else {
				rightOp = String.format(check_gmp_ulong, op2Name);
			}
		} else {
			if(thisLength == 1) {
				rightOp = String.format(check_uchar, op2Name, rsign);
			} else if(thisLength == 2) {
				rightOp = String.format(check_ushort, op2Name, rsign);
			} else if(thisLength == 4) {
				rightOp = String.format(check_uint, op2Name, rsign);
			} else {
				rightOp = String.format(check_ulong, op2Name, rsign);
			}
		}
		
		return (leftOp + " " + optrString + " " + rightOp);
	}
	
	// RETURN: new code to be inserted
	private String handleBitOperationWithAssign(IASTExpression operand1, IASTExpression operand2, String op1Name, String op2Name, 
			Map<String, Boolean> tempVarMap, Set<String> liftedVars, Map<String, String> typeAlias, String optrString) {
		Pair<Integer, Boolean> ltype = getFinalType(operand1, typeAlias);
		Pair<Integer, Boolean> rtype = getFinalType(operand2, typeAlias);
		String lsign = ltype.getSecond() ? "1" : "0";
		String rsign = rtype.getSecond() ? "1" : "0";
		int thisLength = ltype.getFirst();
		String leftOp = ""; 
		String rightOp = "";
		boolean leftHP = false;
		String addCode = "";
		
		// discuss two operands respectively
		if(isHighPrecisionVar(op1Name, tempVarMap, liftedVars)) {
			if(thisLength == 1) {
				leftOp = String.format(check_gmp_uchar, op1Name);
			} else if(thisLength == 2) {
				leftOp = String.format(check_gmp_ushort, op1Name);
			} else if(thisLength == 4) {
				leftOp = String.format(check_gmp_uint, op1Name);
			} else {
				leftOp = String.format(check_gmp_ulong, op1Name);
			}
			leftHP = true;
		} else {
			if(thisLength == 1) {
				leftOp = String.format(check_uchar, op1Name, lsign);
			} else if(thisLength == 2) {
				leftOp = String.format(check_ushort, op1Name, lsign);
			} else if(thisLength == 4) {
				leftOp = String.format(check_uint, op1Name, lsign);
			} else if(thisLength == 8) {
				leftOp = String.format(check_ulong, op1Name, lsign);
			}
			leftHP = false;
		}
		
		if(isHighPrecisionVar(op2Name, tempVarMap, liftedVars)) {
			if(thisLength == 1) {
				rightOp = String.format(check_gmp_uchar, op2Name);
			} else if(thisLength == 2) {
				rightOp = String.format(check_gmp_ushort, op2Name);
			} else if(thisLength == 4) {
				rightOp = String.format(check_gmp_uint, op2Name);
			} else {
				rightOp = String.format(check_gmp_ulong, op2Name);
			}
		} else {
			if(thisLength == 1) {
				rightOp = String.format(check_uchar, op2Name, rsign);
			} else if(thisLength == 2) {
				rightOp = String.format(check_ushort, op2Name, rsign);
			} else if(thisLength == 4) {
				rightOp = String.format(check_uint, op2Name, rsign);
			} else {
				rightOp = String.format(check_ulong, op2Name, rsign);
			}
		}
		
		if(leftHP) {
			addCode = addCode.concat(String.format(mpz_set_ui, op1Name, leftOp + " " + optrString + " " + rightOp)).concat("; ");
		} else {
			addCode = addCode.concat(op1Name).concat(" = ").concat(leftOp + " " + optrString + " " + rightOp).concat("; ");
		}
		
		return addCode;
	}
	
	private Pair<Integer, Boolean> getFinalType(IASTDeclSpecifier spec, Map<String, String> typeAlias) {
		if(spec instanceof IASTSimpleDeclSpecifier) {
			String specString = spec.getRawSignature();
			return getTypeDetailedInfo(specString);
		} else if(spec instanceof IASTEnumerationSpecifier) {
			// enum DAY { mon, ..., sun } today, yesterday;
			return Pair.of(4, true);
		} else if(spec instanceof IASTElaboratedTypeSpecifier) {
			// enum DAY today, yesterday;
			if(((IASTElaboratedTypeSpecifier) spec).getKind() == IASTElaboratedTypeSpecifier.k_enum) {
				return Pair.of(4, true);
			} else {
				return Pair.of(-1, false);
			}
		} else if(spec instanceof IASTNamedTypeSpecifier) {
			// this case handles typedef statement
			String specString = spec.getRawSignature();
			while(true) {
				String name = typeAlias.get(specString);
				if(name == null) {
					return Pair.of(-1, false);
				} else {
					Pair<Integer, Boolean> eval = getTypeDetailedInfo(name);
					if(eval.getFirst() == -1) {
						// this type is not numerical yet
						specString = name; // continue searching
					} else {
						return eval;
					}
				}
			}
		} else {
			return Pair.of(-1, false);
		}
	}
	
	private Pair<Integer, Boolean> getFinalType(IType type, Map<String, String> typeAlias) {
		if(type instanceof IBasicType) {
			String typeString = type.toString();
			return getTypeDetailedInfo(typeString);
		} else if(type instanceof IEnumeration) {
			return Pair.of(4, true); // enumerator can only be of int type
		} else if(type instanceof IQualifierType) {
			// a type can be decorated by const or volatile
			return getFinalType(((IQualifierType)type).getType(), typeAlias);
	  	} else if(type instanceof ITypedef) {
			String typeString = ((ITypedef) type).getType().toString();
			Pair<Integer, Boolean> preEval = getTypeDetailedInfo(typeString);
			if(preEval.getFirst() != -1) {
				return preEval;
			}
			while(true) {
				String name = typeAlias.get(typeString);
				if(name == null) {
					return Pair.of(-1, false);
				} else {
					// we find an alias of this type
					Pair<Integer, Boolean> eval = getTypeDetailedInfo(name);
					if(eval.getFirst() == -1) {
						// this one is not a integer type, continue search...
						typeString = name;
					} else {
						return eval;
					}
				}
			}
		} else {
			// by default this value is unsigned since pointer is unsigned
			// ** On Linux64 platform, pointer has the type of unsigned long
			return Pair.of(-1, false);
		}
	}
	
	private Pair<Integer, Boolean> getFinalType(IASTExpression expr, Map<String, String> typeAlias) {
		IType type = expr.getExpressionType();
		return getFinalType(type, typeAlias);
	}
	
	private String[] deleteQualifier(String[] words) {
		List<String> wordList = new ArrayList<>(Arrays.asList(words));
		List<String> newWordList = new ArrayList<>();
		for(int i = 0; i < wordList.size(); i++) {
			String thisWord = wordList.get(i);
			if(thisWord.isEmpty()) {
				continue; // we don't need empty words here
			} else if(thisWord.equals("const") || thisWord.equals("volatile") || thisWord.equals("restrict")) {
				continue; // we don't need qualifiers
			} else {
				newWordList.add(thisWord);
			}
		}
		
		return newWordList.toArray(new String[newWordList.size()]);
	}
	
	private Pair<Integer, Boolean> getTypeDetailedInfo(String typeString) {
		int length = -1;
		boolean isSigned = true;
		String[] words = typeString.split(" ");
		int startIndex = 0;
		
		// remove qualifiers from type string
		words = deleteQualifier(words);
		
		if(words.length == 0) {
			return Pair.of(-1, false);
		}
		
		String firstWord = words[0];
		if(firstWord.equals("unsigned")) {
			isSigned = false;
			startIndex++;
		} else if(firstWord.equals("signed")) {
			isSigned = true;
			startIndex++;
		} else {
			if(firstWord.equals("char")) {
				length = 1;
			} else if(firstWord.equals("short")) {
				length = 2;
			} else if(firstWord.equals("int")) {
				length = 4;
			} else if(firstWord.equals("long")) {
				length = 8;
			} else {
				length = -1;
			}
			return Pair.of(length, isSigned);
		}
		
		while(startIndex < words.length) {
			String secWord = words[startIndex];
			if(secWord.equals("")) {
				startIndex++;
			} else {
				if(secWord.equals("char")) {
					length = 1;
				} else if(secWord.equals("short")) {
					length = 2;
				} else if(secWord.equals("int")) {
					length = 4;
				} else if(secWord.equals("long")) {
					length = 8;
				} else {
					length = -1;
				}
				return Pair.of(length, isSigned);
			}
		}
		
		// if you can reach here, you cannot find the second non-empty word
		length = 4;
		return Pair.of(length, isSigned);
	}
	
	private String[] getMapDelta(Map<String, Boolean> after, Map<String, Boolean> before) {
		// since map of temporary variables is flexible, we assume that key set of "before" is the subset of key set of "after"
		List<String> delta = new ArrayList<String>();
		for(String key : after.keySet()) {
			// if some entries in "after" are not in "before", get method will return null
			if(after.get(key) != null && before.get(key) != null && after.get(key) == false && before.get(key) == true) {
				delta.add(key);
			}
		}
		return delta.toArray(new String[delta.size()]);
	}
	
	private void recycleVar(String varName, Map<String, Boolean> varTable) {
		varTable.replace(varName, true);
	}
	
	private void recycleVars(String[] vars, Map<String, Boolean> varTable) {
		for(String var : vars) {
			varTable.replace(var, true);
		}
	}
	
	private String getVar(Map<String, Boolean> varTable) {
		for(Entry<String, Boolean> entry : varTable.entrySet()) {
			if(entry.getValue() == true) {
				String newVar = entry.getKey();
				// add inconsistent check here!
				if(tempVarFrozen.contains(newVar)) {
					continue;
				}
				varTable.replace(newVar, false);
				tempVarInScope.add(newVar); // elements in tempVarInScope are cleared only when we proceed from current scope to another one
				return newVar;
			}
		}
		// If all the temporary variables are occupied now, extend this table for a chunk (size = 5 by default)
		int origSize = varTable.size();
		for(int i = 1; i <= tempVarDelta; i++) {
			String varName = tempPrefix + String.valueOf(origSize + i);
			varTable.put(varName, true);
		}
		// update here!!
		tempVarNum += tempVarDelta;
		// get the first new added element
		String newVar = tempPrefix + String.valueOf(origSize + 1);
		varTable.replace(newVar, false);
		tempVarInScope.add(newVar);
		return newVar;
	}
	
	// some of temporary variables are locked in the range of wider block
	private void reinitializeVarMap(Map<String, Boolean> varTable) {
		for(Entry<String, Boolean> entry : varTable.entrySet()) {
			String key = entry.getKey();
			varTable.put(key, true);
		}
		// add variable freezing to prevent potential memory leak
		for(String frozenVar : tempVarFrozen) {
			varTable.put(frozenVar, false);
		}
	}
	
	private <S, T> void copyMapFromTo(Map<S, T> from, Map<S, T> to) {
		// ensure that destination map is empty
		to.clear();
		for(Entry<S, T> entry : from.entrySet()) {
			to.put(entry.getKey(), entry.getValue());
		}
		assert (to.size() == from.size());
	}
	
	private <T> void copySetFromTo(Set<T> from, Set<T> to) {
		to.clear();
		Iterator<T> iterator = from.iterator();
		while(iterator.hasNext()) {
			to.add(iterator.next());
		}
		assert (from.size() == to.size());
	}
	
	private String handleSimpleDeclaration(IASTSimpleDeclaration declNode, Map<String, String> typeAlias, Set<String> liftedVars, boolean isGlobal) {
		// a declaration has two key components: specifier and declarator
		IASTDeclSpecifier specifier = declNode.getDeclSpecifier();
		IASTDeclarator[] declarators = declNode.getDeclarators();
		// first we discuss different specifiers
		if(specifier instanceof IASTSimpleDeclSpecifier) {
			// the most common case
			// (1) if it is a typedef clause, we add an entry into typeAlias
			// (2) otherwise, it is a simple declaration, 
			if(specifier.getStorageClass() == IASTDeclSpecifier.sc_typedef) {
				String originType = specifier.getRawSignature().replace("typedef", "").trim();
				assert (declarators.length == 1);
				String alias = declarators[0].getName().toString();
				// one origin type may corresponds to multiple aliases
				typeAlias.put(alias, originType);
				return declNode.getRawSignature();
			} else {
				// we keep global declarations
				if(isGlobal) {
					// return raw signature will keep some preprocessing garbage in the code, which is undesirable
					/*
					 * How to eliminate preprocessing flags in the code?
					 * - For example, NULL is defined in stdlib.h. If we preprocess the code, NULL will be replaced with ((void*)0) surrounding with some preprocessing
					 * - directives. If we analyze this expression, these directives are eliminated automatically
					 */
					String declString = "";
					// declarators should be processed individually
					String specString = specifier.getRawSignature();
					for(IASTDeclarator declarator : declarators) {
						String declName = declarator.getName().getRawSignature();
						IASTInitializer initializer = declarator.getInitializer();
						if(initializer != null) {
							assert (initializer instanceof IASTEqualsInitializer);
							IASTInitializerClause clause = ((IASTEqualsInitializer)initializer).getInitializerClause();
							declString = declString.concat(specString).concat(" ").concat(declName).concat(" = ").concat(clause.getRawSignature()).concat(";\n");
						} else {
							declString = declString.concat(specString).concat(" ").concat(declName).concat(";\n");
						}
					}
					return declString;
				} else {
					return handleSimpleDeclSpecifier(declNode, (IASTSimpleDeclSpecifier)specifier, declarators, typeAlias, liftedVars);
				}
			}
		} else if(specifier instanceof IASTNamedTypeSpecifier) {
			// target type is a type alias, analogously there are 2 cases:
			// (1) if it is a typedef clause, we add an entry into typeAlias
			// (2) otherwise, it is a simple declaration
			if(specifier.getStorageClass() == IASTDeclSpecifier.sc_typedef) {
				String originType = ((IASTNamedTypeSpecifier) specifier).getName().getRawSignature();
				assert (declarators.length == 1);
				String alias = declarators[0].getName().toString();
				typeAlias.put(alias, originType);
				return declNode.getRawSignature();
			} else {
				if(isGlobal) {
					// try to eliminate preprocessing directives
					String declString = "";
					String specString = specifier.getRawSignature();
					for(IASTDeclarator declarator : declarators) {
						String declName = declarator.getName().getRawSignature();
						IASTInitializer initializer = declarator.getInitializer();
						if(initializer != null) {
							assert (initializer instanceof IASTEqualsInitializer);
							IASTInitializerClause clause = ((IASTEqualsInitializer)initializer).getInitializerClause();
							declString = declString.concat(specString).concat(" ").concat(declName).concat(" = ").concat(clause.getRawSignature()).concat(";\n");
						} else {
							declString = declString.concat(specString).concat(" ").concat(declName).concat(";\n");
						}
					}
					return declString;
				} else {
					return handleNamedTypeSpecifier(declNode, (IASTNamedTypeSpecifier)specifier, declarators, typeAlias, liftedVars);
				}
			}	
		} else if(specifier instanceof IASTElaboratedTypeSpecifier) {
			String typeKey;
			int kind = ((IASTElaboratedTypeSpecifier) specifier).getKind();
			if(kind == IASTElaboratedTypeSpecifier.k_struct) {
				typeKey = "struct";
			} else if(kind == IASTElaboratedTypeSpecifier.k_enum) {
				typeKey = "enum";
			} else {
				typeKey = "union";
			}
			if(specifier.getStorageClass() == IASTDeclSpecifier.sc_typedef) {
				String originType = ((IASTElaboratedTypeSpecifier) specifier).getName().getRawSignature();
				originType = typeKey.concat(" ").concat(originType);
				assert (declarators.length == 1);
				String alias = declarators[0].getName().getRawSignature();
				typeAlias.put(alias, originType);
				return declNode.getRawSignature();
			} else {
				// remove variables from liftedVars, since local variables can be overwrite global variables
				removeDeclarators(declarators, liftedVars);
				return declNode.getRawSignature();
			}
		} else if(specifier instanceof IASTCompositeTypeSpecifier) {
			// in this case, we will handle struct or union with detailed definition (therefore it is ``composite")
			String typeKey;
			int key = ((IASTCompositeTypeSpecifier) specifier).getKey();
			if(key == IASTCompositeTypeSpecifier.k_struct) {
				typeKey = "struct";
			} else {
				// should be union
				typeKey = "union";
			}
			if(specifier.getStorageClass() == IASTDeclSpecifier.sc_typedef) {
				// it is possible, for example, typedef struct point { int x,y; } Coordinate;
				String originType = ((IASTCompositeTypeSpecifier) specifier).getName().getRawSignature();
				originType = typeKey.concat(" ").concat(originType);
				assert (declarators.length == 1);
				String alias = declarators[0].getName().getRawSignature();
				typeAlias.put(alias, originType);
				return declNode.getRawSignature();
			} else {
				// remove variables from liftedVars
				removeDeclarators(declarators, liftedVars);
				return declNode.getRawSignature();
			}
		} else if(specifier instanceof IASTEnumerationSpecifier) {
			// in this case, we will handle enum with detailed definition
			if(specifier.getStorageClass() == IASTDeclSpecifier.sc_typedef) {
				String originType = ((IASTEnumerationSpecifier) specifier).getName().getRawSignature();
				originType = "enum " + originType;
				assert (declarators.length == 1);
				String alias = declarators[0].getName().getRawSignature();
				typeAlias.put(alias, originType);
				return declNode.getRawSignature();
			} else {
				// remove variables from liftedVars
				removeDeclarators(declarators, liftedVars);
				return declNode.getRawSignature();
			}
		} else {
			System.err.println("Unsupported syntax element");
			// remove declarators
			removeDeclarators(declarators, liftedVars);
			return declNode.getRawSignature();
		}
	}
	
	// removeDeclarators is necessary only when handling local declarations
	private void removeDeclarators(IASTDeclarator[] declarators, Set<String> liftedVars) {
		for(IASTDeclarator declarator : declarators) {
			String declName = declarator.getName().getRawSignature();
			liftedVars.remove(declName);
		}
	}
	
	private void removeDeclarator(IASTDeclarator declarator, Set<String> liftedVars) {
		String declName = declarator.getName().getRawSignature();
		liftedVars.remove(declName);
	}
	
	private String handleSimpleDeclSpecifier(IASTSimpleDeclaration declNode, IASTSimpleDeclSpecifier specifier, IASTDeclarator[] declarators, Map<String, String> typeAlias, Set<String> liftedVars) {
		boolean intSpec = true;
		if(isNotIntegerSpecifier(specifier)) {
			// keep the original code
			// If we allocate a dynamic array of non-integer type??
			removeDeclarators(declarators, liftedVars);
			intSpec = false;
		}
		// convert to GMP version
		String newCode = "";
		for(IASTDeclarator declarator : declarators) {
			// ignore array and field declarators
			if((declarator instanceof IASTArrayDeclarator) || (declarator instanceof IASTFieldDeclarator) || (declarator instanceof IASTFunctionDeclarator)) {
				removeDeclarator(declarator, liftedVars);
				// keep the code
				String codeLine = specifier.getRawSignature() + " " + declarator.getRawSignature() + "; ";
				newCode = newCode.concat(codeLine);
				continue;
			}
			// wait... we should carefully handle ordinary values and pointers!
			IASTPointerOperator[] asterisk = declarator.getPointerOperators();
			if(asterisk.length == 0 && intSpec) {
				// congratulations! This is a ordinary value declarator
				String newName = declarator.getName().getRawSignature();
				liftedVars.add(newName); // liftedVars contains old, ordinary name
				newName = prefix + newName; // OK, this is the new variable name
				// add a declaration first
				newCode = newCode.concat(mpz_t).concat(" ").concat(newName).concat("; ");
				IASTInitializer initializer = declarator.getInitializer();
				String initValue = "0";
				if(initializer != null) {
					// initial expression is right-hand expression, which should be a constant in pure C!
					/*
					 * There are totally 3 kinds of initializer:
					 * (1) IASTEqualsInitializer, which has the form of ``= expr". If this is an integer variable declaration, it should have this form;
					 * (2) IASTInitializerList, which has the form of ``= {1,2,3}". It is often used to initialize array;
					 * (3) ICASTDesignatedInitializer, which has the form of ``.x=4". It is often used to initialize struct object.
					 */
					assert (initializer instanceof IASTEqualsInitializer);
					// in C99/C11, the initializer is not required to be a constant expression, that means we can use arbitrary type of expression
					//        to initialize certain variable. Then, we should evaluate this expression first
					// PROBLEM: what will happen if the initializer overflows? Don't worry, although at initial the value of original variable is abnormal,
					//          when we are about to use it (mostly on ampersand operation), we will copy the precise value from GMP version of integer, 
					//          then the problem will be detected.
					IASTInitializerClause initLiteral = ((IASTEqualsInitializer)initializer).getInitializerClause();
					IASTExpression initExpr = (IASTExpression)initLiteral;
					// OK, try to handle this first
					Pair<String, String> evalInit = scanExpressionInDFS(initExpr, typeAlias, tempVarTable, liftedVars);
					reinitializeVarMap(tempVarTable);
					// global variable must be initialized with constant, so no extra codes should be introduced
					newCode = newCode.concat(evalInit.getFirst());					
					initValue = evalInit.getSecond();
					// initialization code below						
					if(isHighPrecisionVar(initValue, tempVarTable, liftedVars)) {
						// the value is a GMP integer
						newCode = newCode.concat(String.format(mpz_init, newName));
						newCode = newCode.concat(String.format(mpz_set, newName, initValue)).concat("; ");
					} else {
						// ordinary machine integer
						newCode = newCode.concat(String.format(mpz_init, newName));
						Pair<Integer, Boolean> initType = getFinalType(initExpr, typeAlias);
						if((initType.getSecond() == false && initType.getFirst() == 8) || initType.getFirst() == -1) {
							newCode = newCode.concat(String.format(mpz_set_ui, newName, initValue)).concat("; ");
						} else {
							newCode = newCode.concat(String.format(mpz_set_si, newName, initValue)).concat("; ");
						}
					}
					
					
				} else {
					// then, we don't need to set a initial value --- its initial value is 0 by default
						newCode = newCode.concat(String.format(mpz_init, newName));
				}
				
				// in order to process ampersand(&) operation, we have to keep the original declaration of integer variable
				// hence the new generated code should include 2 declarations --- the original one and the highly precise one
				
				// if the declarator is a function expression, it is possible to call that function twice (unnecessary behaviour)
				String codeLine = specifier.getRawSignature() + " " + declarator.getName().getRawSignature() + "; ";
				newCode = newCode.concat(codeLine);	
				
				// add sign info 
				Pair<Integer, Boolean> specType = getFinalType(specifier, typeAlias);
				signTable.put(newName, specType.getSecond());
				
				// we should record local GMP variables here for clean work before exiting this function
				// **NOTE: this structure is cleared when returning from one function
				localVariables.add(newName);
			} else {
				// keep this piece of code!
				removeDeclarator(declarator, liftedVars);
				
				String codeLine = "";
				String declName = declarator.getName().getRawSignature();
				IASTInitializer initializer = declarator.getInitializer();
				if(initializer != null) {
					assert (initializer instanceof IASTEqualsInitializer);
					IASTInitializerClause clause = ((IASTEqualsInitializer)initializer).getInitializerClause();
					IASTExpression initExpr = (IASTExpression)clause;
					Pair<String, String> evalInit = scanExpressionInDFS(initExpr, typeAlias, tempVarTable, liftedVars);
					reinitializeVarMap(tempVarTable);
					codeLine = codeLine.concat(evalInit.getFirst());
					String evalName = evalInit.getSecond();
					
					codeLine = codeLine.concat(specifier.getRawSignature()).concat(" ");
					for(int i = 0; i < asterisk.length; i++) {
						codeLine = codeLine.concat("*");
					}
					codeLine = codeLine.concat(declName).concat(" = ").concat(evalName).concat("; ");
					
					// OK, handle pointer-to relation now
					if(initExpr instanceof IASTIdExpression) {
						// case 1: p = q
						String pointerVal = pointerInfo.get(evalName);
						if(pointerVal != null) {
							pointerInfo.put(declName, pointerVal);
						}
					} else if (initExpr instanceof IASTUnaryExpression) {
						IASTUnaryExpression uexpr = (IASTUnaryExpression)initExpr;
						int possibleAmper = uexpr.getOperator();
						// case 2: p = &q
						if(possibleAmper == IASTUnaryExpression.op_amper) {
							String lvalue = evalName.substring(1);
							String newvalue = prefix + lvalue;
							if(isLiftedVar(newvalue, liftedVars)) {
								pointerInfo.put(declName, newvalue);
							}
							if(pointerInfo.containsKey(lvalue)) {
								pointerInfo.put(declName, lvalue);
							}
						} else if(possibleAmper == IASTUnaryExpression.op_star) {
							// case 3: p = *(*)q
							// its operand must be a pointer
							String starVal = evalName;
							int refLevel = 0;
							while(starVal.charAt(0) == '*') {
								starVal = starVal.substring(1);
								starVal = deBracket(starVal);
								refLevel++;
							}
							while(refLevel > 0) {
								starVal = pointerInfo.get(starVal);
								if(starVal == null) {
									break;
								}
								refLevel--;
							}
							if(refLevel == 0) {
								starVal = pointerInfo.get(starVal);
								if(starVal != null) {
									pointerInfo.put(declName, starVal);
								}
							}
						}
					}
				} else {
					codeLine = codeLine.concat(specifier.getRawSignature()).concat(" ");
					for(int i = 0; i < asterisk.length; i++) {
						codeLine = codeLine.concat("*");
					}
					codeLine = codeLine.concat(declName).concat(";");
				}
				
				newCode = newCode.concat(codeLine);
			}
		}
		return newCode;
	}
	
	private String handleNamedTypeSpecifier(IASTSimpleDeclaration declNode, IASTNamedTypeSpecifier specifier, IASTDeclarator[] declarators, Map<String, String> typeAlias, Set<String> liftedVars) {
		Boolean signAI = signOfIntegerAlias(specifier, typeAlias);
		if(!signAI) {
			removeDeclarators(declarators, liftedVars);
		}
		String newCode = "";
		for(IASTDeclarator declarator : declarators) {
			// ignore array and field declarators
			if((declarator instanceof IASTArrayDeclarator) || (declarator instanceof IASTFieldDeclarator) || (declarator instanceof IASTFunctionDeclarator)) {
				removeDeclarator(declarator, liftedVars);
				// keep the code
				String codeLine = specifier.getRawSignature() + " " + declarator.getRawSignature() + "; ";
				newCode = newCode.concat(codeLine);
				continue;
			}
			IASTPointerOperator[] asterisk = declarator.getPointerOperators();
			if(asterisk.length == 0 && signAI) {
				String newName = declarator.getName().getRawSignature();
				liftedVars.add(newName);
				newName = prefix + newName;
				// add a declaration first
				newCode = newCode.concat(mpz_t).concat(" ").concat(newName).concat("; ");
				IASTInitializer initializer = declarator.getInitializer();
				String initValue = "0";
				if(initializer != null) {
					assert (initializer instanceof IASTEqualsInitializer);
					// the right-hand value must be literal expression, try to get its value
					IASTInitializerClause initLiteral = ((IASTEqualsInitializer)initializer).getInitializerClause();
					if(!(initLiteral instanceof IASTExpression)) {
						// how to handle this case?
						newCode = newCode.concat(declNode.getRawSignature()).concat("; ");
						return newCode;
					}
					IASTExpression initExpr = (IASTExpression)initLiteral;
					// OK, try to handle this first
					Pair<String, String> evalInit = scanExpressionInDFS(initExpr, typeAlias, tempVarTable, liftedVars);
					reinitializeVarMap(tempVarTable);
					newCode = newCode.concat(evalInit.getFirst());
					initValue = evalInit.getSecond();
					
					if(isHighPrecisionVar(initValue, tempVarTable, liftedVars)) {
						// the value is a GMP integer
						newCode = newCode.concat(String.format(mpz_init, newName));
						newCode = newCode.concat(String.format(mpz_set, newName, initValue)).concat("; ");
					} else {
						// ordinary machine integer
						newCode = newCode.concat(String.format(mpz_init, newName));
						Pair<Integer, Boolean> initType = getFinalType(initExpr, typeAlias);
						if((initType.getSecond() == false && initType.getFirst() == 8) || initType.getFirst() == -1) {
							newCode = newCode.concat(String.format(mpz_set_ui, newName, initValue)).concat("; ");
						} else {
							newCode = newCode.concat(String.format(mpz_set_si, newName, initValue)).concat("; ");
						}
					}
				} else {
					newCode = newCode.concat(String.format(mpz_init, newName));
				}
				
				String codeLine = specifier.getRawSignature() + " " + declarator.getName().getRawSignature() + "; ";
				newCode = newCode.concat(codeLine);
				
				signTable.put(newName, signAI.booleanValue());
				
				// **NOTE: prepare to be disposed when leaving this scope
				localVariables.add(newName);
			} else {
				removeDeclarator(declarator, liftedVars);
				String codeLine = "";
				String declName = declarator.getName().getRawSignature();
				IASTInitializer initializer = declarator.getInitializer();
				if(initializer != null) {
					assert (initializer instanceof IASTEqualsInitializer);
					IASTInitializerClause clause = ((IASTEqualsInitializer)initializer).getInitializerClause();
					if(!(clause instanceof IASTExpression)) {
						// we do not handle this case
						codeLine = codeLine.concat(declNode.getRawSignature()).concat("; ");
						return codeLine;
					}
					IASTExpression initExpr = (IASTExpression)clause;
					Pair<String, String> evalInit = scanExpressionInDFS(initExpr, typeAlias, tempVarTable, liftedVars);
					reinitializeVarMap(tempVarTable);
					codeLine = codeLine.concat(evalInit.getFirst());
					String evalName = evalInit.getSecond();
					
					codeLine = codeLine.concat(specifier.getRawSignature()).concat(" ");
					for(int i = 0; i < asterisk.length; i++) {
						codeLine = codeLine.concat("*");
					}
					codeLine = codeLine.concat(declName).concat(" = ").concat(evalName).concat("; ");
					
					// OK, handle pointer-to relation now
					if(initExpr instanceof IASTIdExpression) {
						// case 1: p = q
						String pointerVal = pointerInfo.get(evalName);
						if(pointerVal != null) {
							pointerInfo.put(declName, pointerVal);
						}
					} else if (initExpr instanceof IASTUnaryExpression) {
						IASTUnaryExpression uexpr = (IASTUnaryExpression)initExpr;
						int possibleAmper = uexpr.getOperator();
						// case 2: p = &q
						if(possibleAmper == IASTUnaryExpression.op_amper) {
							String lvalue = evalName.substring(1);
							String newvalue = prefix + lvalue;
							if(isLiftedVar(newvalue, liftedVars)) {
								pointerInfo.put(declName, newvalue);
							}
							if(pointerInfo.containsKey(lvalue)) {
								pointerInfo.put(declName, lvalue);
							}
						} else if(possibleAmper == IASTUnaryExpression.op_star) {
							// case 3: p = *(*)q
							// its operand must be a pointer
							String starVal = evalName;
							int refLevel = 0;
							while(starVal.charAt(0) == '*') {
								starVal = starVal.substring(1);
								starVal = deBracket(starVal);
								refLevel++;
							}
							while(refLevel > 0) {
								starVal = pointerInfo.get(starVal);
								if(starVal == null) {
									break;
								}
								refLevel--;
							}
							if(refLevel == 0) {
								starVal = pointerInfo.get(starVal);
								if(starVal != null) {
									pointerInfo.put(declName, starVal);
								}
							}
						}
					}
				} else {
					codeLine = codeLine.concat(specifier.getRawSignature()).concat(" ");
					for(int i = 0; i < asterisk.length; i++) {
						codeLine = codeLine.concat("*");
					}
					codeLine = codeLine.concat(declName).concat(";");
				}
				
				newCode = newCode.concat(codeLine);
			}
		}
		return newCode;
	}
	
	private boolean isAliasOfInteger(IASTNamedTypeSpecifier spec, Map<String, String> typeAlias) {
		String typeString = spec.getName().getRawSignature();
		while(true) {
			String alias = typeAlias.get(typeString);
			if(alias == null) {
				return false;
			} else {
				// check if it is an integer specifier
				Pair<Integer, Boolean> eval = getTypeDetailedInfo(alias);
				if(eval.getFirst() != -1) {
					return true;
				} else {
					// further check is required
					typeString = alias;
				}
			}
		}
	}
	
	private boolean signOfIntegerAlias(IASTNamedTypeSpecifier spec, Map<String, String> typeAlias) {
		String typeString = spec.getName().getRawSignature();
		while(true) {
			String alias = typeAlias.get(typeString);
			if(alias == null) {
				// we assume this specifier is not integer type
				return false;
			} else {
				// check if it is an integer specifier
				Pair<Integer, Boolean> eval = getTypeDetailedInfo(alias);
				if(eval.getFirst() != -1) {
					return eval.getSecond();
				} else {
					// further check is required
					typeString = alias;
				}
			}
		}
	}
	
	private boolean isNotIntegerSpecifier(IASTSimpleDeclSpecifier spec) {
		String typeString = spec.getRawSignature();
		// the original implementation is problematic
		Pair<Integer, Boolean> typeInfo = getTypeDetailedInfo(typeString);
		int length = typeInfo.getFirst();
		if(length != -1) {
			return false;
		} else {
			// it is impossible for a simple declaration specifier to be an alias of one defined type
			return true;
		}
	}

	private boolean isGlobalVariableDeclaration(IASTSimpleDeclaration decl) {
		// traverse its children to see if there is IASTFunctionDeclarator
		IASTNode[] children = decl.getChildren();
		for(IASTNode child : children) {
			if(child instanceof IASTFunctionDeclarator) {
				return false;
			} 
		}
		return true;
	}
	
	private String destroyGenerate(String indentStr) {
		// besides them, we should destroy GMP variables in current scope, too!!
		Set<String> totalLocalVarSet = new HashSet<>();
		
		for(Set<String> localVars : localVariableStack) {
			totalLocalVarSet.addAll(localVars);
		}
		
		String outputCode = "";
		for(String tempVar : tempVarFrozen) {
			outputCode = outputCode.concat(indentStr).concat(String.format(mpz_clear, tempVar)).concat("\n");
		}
		for(String tempVar : tempVarInScope) {
			outputCode = outputCode.concat(indentStr).concat(String.format(mpz_clear, tempVar)).concat("\n");
		}
		for(String localVar : totalLocalVarSet) {
			outputCode = outputCode.concat(indentStr).concat(String.format(mpz_clear, localVar)).concat("\n");
		}
		for(String localVar : localVariables) {
			outputCode = outputCode.concat(indentStr).concat(String.format(mpz_clear, localVar)).concat("\n");
		}
		
		// DON'T clear stack! Since there could be some code to be reached has not been analyzed yet.
		return outputCode;
	}
	
	private CFormatStringParseResult[] parseCFormatString(String formatString) {
		
		// FIRST, recognize LEGAL format string patterns and analyze them with methods below
		// SECOND, organize parse results into an array
		
		char[] formatCharArray = formatString.toCharArray();
		List<CFormatStringParseResult> parseResults = new ArrayList<>();
		int index = 0;
		int absoluteStartIndex = 0;
		while(index < formatCharArray.length) {
			char currentChar = formatCharArray[index];
			if(currentChar != '%') {
				index++;
				continue;
			} else {
				// possible format string pattern here, let's trace it!
				absoluteStartIndex = index;
				index++;
				if(index >= formatCharArray.length) {
					// unhealthy pattern, but acceptable
					break;
				}
				
				char nextChar = formatCharArray[index];
				if(nextChar == '-' || nextChar == '+' || nextChar == ' ' || nextChar == '#' || nextChar == '0') {
					// flag exists
					index++;
					if(index >= formatCharArray.length) {
						break;
					}
				}
				
				nextChar = formatCharArray[index];
				if(nextChar == '*') {
					// width exists and is *
					index++;
					if(index >= formatCharArray.length) {
						break;
					}
				} else {
					while(index < formatCharArray.length) {
						char wchar = formatCharArray[index];
						if(wchar >= '0' && wchar <= '9') {
							index++;
						} else {
							break;
						}
					}
					if(index >= formatCharArray.length) {
						break;
					}
				}
				
				nextChar = formatCharArray[index];
				if(nextChar == '.') {
					// precision part exists
					// must be * or number, otherwise this pattern is illegal
					index++;
					if(index >= formatCharArray.length) {
						break;
					}
					char nnextChar = formatCharArray[index];
					if(nnextChar == '*') {}
					else if(nnextChar >= '0' && nnextChar <= '9') {
						index++;
						while(index < formatCharArray.length) {
							char pchar = formatCharArray[index];
							if(pchar >= '0' && pchar <= '9') {
								index++;
							} else {
								break;
							}
						}
						if(index >= formatCharArray.length) {
							break;
						}
					} else {
						continue;
					}
				}
				
				nextChar = formatCharArray[index];
				if(nextChar == 'h') {
					index++;
					if(index >= formatCharArray.length) {
						break;
					}
					if(formatCharArray[index] == 'h') {
						index++;
						if(index >= formatCharArray.length) {
							break;
						}
					} 
				} else if(nextChar == 'l') {
					index++;
					if(index >= formatCharArray.length) {
						break;
					}
					if(formatCharArray[index] == 'l') {
						index++;
						if(index >= formatCharArray.length) {
							break;
						}
					}
				} else if(nextChar == 'j' || nextChar == 'z' || nextChar == 't' || nextChar == 'L') {
					index++;
					if(index >= formatCharArray.length) {
						break;
					}
				}
				
				// the last one is compulsory: if inconsistent, this pattern has the mal-form
				nextChar = formatCharArray[index];
				if(legalSpecifier.indexOf(nextChar) != -1) {
					// legal! add it into parseResult
					index++;
					String formatPattern = formatString.substring(absoluteStartIndex, index);
					CFormatStringParseResult result = new CFormatStringParseResult(formatPattern);
					parseResults.add(result);
				} else {
					// malform, ignore it now!
					index++;
					continue;
				}
			}
		}
		
		return parseResults.toArray(new CFormatStringParseResult[parseResults.size()]);
		
	}
	
	private class CFormatStringParseResult {
		
		public static final int f_neg = 1;
		public static final int f_pos = 2;
		public static final int f_space = 3;
		public static final int f_pound = 4;
		public static final int f_zero = 0;
		public static final int f_none = -1;
		
		public static final int w_custom = -1;
		public static final int w_none = -2;
		
		public static final int p_custom = -1;
		public static final int p_none = -2;
		
		public static final int l_char = 1; // hh
		public static final int l_short = 2; // h
		public static final int l_int = 4; // none, or other cases
		public static final int l_long = 8; // l
		public static final int l_llong = 8; // ll
		
		public static final int s_sizet = 2; // special case for size_t
		public static final int s_signed = 1;
		public static final int s_unsigned = 0;
		public static final int s_ni = -1; // not an integer
		
		private int flag;
		private int width;
		private int precision;
		private int length;
		private int signed;
		
		/*
		 * assume that input string is a legal format string --- extra check is required outside this function
		 */
		public CFormatStringParseResult(String stringToBeParsed) {
			// input string has the format of %[flag][width][.precision][length]specifier
			// we directly initialize 5 domains of this class according to input string
			
			char[] parseArray = stringToBeParsed.toCharArray();
			assert (parseArray[0] == '%');
			assert (parseArray.length >= 2) : "Invalid format string!";
			int index = 1;
			char currentChar = parseArray[index];
			switch (currentChar) {
			case '-': flag = f_neg; index++; break;
			case '+': flag = f_pos; index++; break;
			case ' ': flag = f_space; index++; break;
			case '#': flag = f_pound; index++; break;
			case '0': flag = f_zero; index++; break;
			default:  flag = f_none;
			}
			
			currentChar = parseArray[index];
			if(currentChar == '*') {
				width = w_custom;
				index++;
			} else {
				int widthStart = index;
				int widthEnd = index;
				while(index < parseArray.length) {
					// read the numbers composing width parameter
					char thisChar = parseArray[index];
					if(thisChar >= '0' && thisChar <= '9') {
						index++;
						widthEnd++;
					} else {
						break;
					}
				}
				if(widthStart == widthEnd) {
					width = w_none;
				} else {
					String widthString = stringToBeParsed.substring(widthStart, widthEnd);
					width = Integer.valueOf(widthString); // no exception guaranteed
				}
			}
			
			currentChar = parseArray[index];
			if(currentChar == '.') {
				// Good, then precision parameter is also specified
				index++;
				int precStart = index;
				int precEnd = index;
				while(index < parseArray.length) {
					char thisChar = parseArray[index];
					if(thisChar >= '0' && thisChar <= '9') {
						index++;
						precEnd++;
					} else {
						break;
					}
				}
				if(precStart == precEnd) {
					assert (parseArray[index] == '*') : "Illegal format string!";
					precision = p_custom;
				} else {
					String precString = stringToBeParsed.substring(precStart, precEnd);
					precision = Integer.valueOf(precString); // no exception guaranteed
				}
			} else {
				precision = p_none;
			}
			
			currentChar = parseArray[index];
			switch(currentChar) {
			case 'h': {
				// must be in the bound of array, otherwise it is not a legal format string
				index++;
				if(parseArray[index] == 'h') {
					index++;
					length = l_char;
				} else {
					length = l_short;
				}
				break;
			}
			case 'l': {
				index++;
				if(parseArray[index] == 'l') {
					index++;
					length = l_llong;
				} else {
					length = l_long;
				}
				break;
			}
			case 'z': length = l_long; signed = s_sizet; index++; break;
			case 'j':
			case 't':
			case 'L': length = l_int; index++; break;
			default: length = l_int; // none for sure, don't increment the index!
			}
			
			currentChar = parseArray[index];
			// the final part is specifier, which is always required
			switch(currentChar) {
			case 'd':
			case 'i': { 
				if(signed != s_sizet) {
					signed = s_signed;
				}
				break;
			}
			case 'u':
			case 'o':
			case 'x':
			case 'X': {
				if(signed != s_sizet) {
					signed = s_unsigned; 
				}
				break;
			}
			case 'c': signed = s_signed; length = l_char; break;
			default: signed = s_ni; // not an integer
			}
			
		}
		
		public int getLength() {
			return this.length;
		}
		
		public int getSigned() {
			return this.signed;
		}
		
		public boolean isCustomWidth() {
			return (this.width == w_custom);
		}
		
		public boolean isCustomPrecision() {
			return (this.precision == p_custom);
		}
		
	}
	
	private class CPreprocessor {
		
		private String inputFileName;
		private String inputIFileName;
		private String inputCode;
		private final Map<Integer, String> mapping = new HashMap<>();
		private final List<String> includeFiles = new ArrayList<>();
		private final Set<String> includeFileNames = new HashSet<>();
		private static final String cpp_program = "cpp";
		private static final String cpp_generated_code = "cpp_generated_code";
		
		public CPreprocessor(String fileName, String procedFileName) {
			this.inputFileName = fileName;
			this.inputIFileName = procedFileName;
		}
		
		private void includeReader() {
			File CFile = new File(inputFileName);
			if(!CFile.exists()) {
				System.err.println("Input file does not exist!");
				return;
			}
			
			FileContent content = FileContent.createForExternalFileLocation(inputFileName);
			Map definedSymbols = new HashMap<>();
			String[] includePaths = new String[0];
			IScannerInfo info = new ScannerInfo(definedSymbols, includePaths);
			IParserLogService log = new DefaultLogService();
			IncludeFileContentProvider emptyInclude = IncludeFileContentProvider.getEmptyFilesProvider();
			try {
				IASTTranslationUnit transUnit = GCCLanguage.getDefault().getASTTranslationUnit(content, info, emptyInclude, null, 8, log);
				IASTPreprocessorIncludeStatement[] includeStmts = transUnit.getIncludeDirectives();
				for(IASTPreprocessorIncludeStatement includeStmt : includeStmts) {
					boolean activeStatus = includeStmt.isActive();
					if(activeStatus) {
						includeFiles.add(includeStmt.toString());
						includeFileNames.add(includeStmt.getName().toString());
					}
				}
			} catch (CoreException e) {
				e.printStackTrace();
			}
		}
		
		public boolean includeFileExists(String inclDirective) {
			String fileName = inclDirective.replace("include", "").replace("#", "").replace("\"", "").replace("<", "").replace(">", "").trim();
			return includeFileNames.contains(fileName);
		}
		
		private void CReader() {
			
			File IFile = new File(inputIFileName);
			if(!IFile.exists()) {
				System.err.println("Input file does not exist!");
				return;
			}
			
			// Then, load this file for further processing
			StringBuilder codeBuilder = new StringBuilder();
			BufferedReader breader = null;
			try {
				breader = new BufferedReader(new FileReader(inputIFileName));
				String codeLine = "";
				while((codeLine = breader.readLine()) != null) {
					codeBuilder.append(codeLine);
					codeBuilder.append('\n');
				}
			} catch(IOException e) {
				e.printStackTrace();
				System.exit(1);
			} finally {
				try {
					if(breader != null) {
						breader.close();
					}
				} catch(Exception e) {
					e.printStackTrace();
					System.exit(1);
				}
			}
			
			inputCode = codeBuilder.toString();
			
		}
		
		public String getContainingFile(int lineNumber) {
			String fileName = mapping.get(lineNumber);
			if(fileName == null) {
				return null; // that means, no such line number
			}
			return fileName;
		}
		
		public String[] getIncludeStatements() {
			return includeFiles.toArray(new String[includeFiles.size()]);
		}
		
		public String processCode() {
			
			// FIRST of the FIRST, we pre-process the code and load it as string
			includeReader();
			
			CReader();
			
			LexerOptions options = new LexerOptions();
			ILexerLog log = ILexerLog.NULL;
			Object source = null;
			Lexer lx = new Lexer(inputCode.toCharArray(), options, log, source);
			
			try {
				/*
				 * (startLineNumber, endLineNumber]
				 */
				int absoluteLineNumber = 1;
				String origFileName = inputFileName;
				int includeStartedWithAbsoluteLine = 1;
				Token token;
				
				// now we scan the code by tokens, and mapping codeline to filename
				while((token = lx.nextToken()).getType() != Token.tEND_OF_INPUT) {
					if(token.getType() == Lexer.tNEWLINE) {
						absoluteLineNumber++; // current line number
					}
					
					if(token.getType() == Token.tPOUND) {
						// handle #
						ArrayList<Token> directiveTokens = Lists.newArrayList();
						token = lx.nextToken();
						while(token.getType() != Lexer.tNEWLINE && token.getType() != Token.tEND_OF_INPUT) {
							directiveTokens.add(token);
							token = lx.nextToken();
						}
						// if we exit the loop above, we must meet a newline or reach the end of the file
						absoluteLineNumber++;
						
						if(directiveTokens.size() > 0) {
							String firstTokenImage = directiveTokens.get(0).getImage().trim();
							final int lineNumberTokenIndex;
							if(directiveTokens.size() > 1 && firstTokenImage.equals("line") && directiveTokens.get(1).getImage().matches("[0-9]+")) {
								lineNumberTokenIndex = 1;
							} else if(firstTokenImage.matches("[0-9]+")) {
								lineNumberTokenIndex = 0;
							} else {
								lineNumberTokenIndex = -1;
							}
							if(lineNumberTokenIndex >= 0) {
								// then we should add entries to location map
								for(int i = includeStartedWithAbsoluteLine; i < absoluteLineNumber - 1; i++) {
									mapping.put(i, origFileName);
								}
								mapping.put(absoluteLineNumber - 1, cpp_generated_code);
								
								includeStartedWithAbsoluteLine = absoluteLineNumber; // the next start point locates here
								if(directiveTokens.size() > lineNumberTokenIndex + 1) {
									// that means we can extract the filename of include header file
									String file = directiveTokens.get(lineNumberTokenIndex + 1).getImage().trim();
									if(file.charAt(0) == '"' && file.charAt(file.length() - 1) == '"') {
										// extract the real filename
										file = file.substring(1, file.length() - 1); // remove the head and tail
									}
									origFileName = file; // update file name
								}
							}
						}
					}
				}
				
				// if we reach the end of the file, we should map remaining code lines to fileName
				for(int i = includeStartedWithAbsoluteLine; i <= absoluteLineNumber; i++) {
					mapping.put(i, origFileName);
				}
			} catch(OffsetLimitReachedException e) {
				e.printStackTrace();
				System.exit(1);
			}
			
			return inputCode;
		}
		
	}
	
}
