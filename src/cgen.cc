//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully and add code to build an LLVM program 
//**************************************************************

#define EXTERN
#include "cgen.h"
#include <string>
#include <sstream>

// 
extern int cgen_debug;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.  Feel free to add your
// own definitions as you see fit.
//
//////////////////////////////////////////////////////////////////////
EXTERN Symbol 
	// required classes
	Object,
	IO,
	String,
	Int,
	Bool,
	Main,

	// class methods
	cool_abort,
	type_name,
	cool_copy,
	out_string,
	out_int,
	in_string,
	in_int,
	length,
	concat,
	substr,

	// class members
	val,

	// special symbols
	No_class,    // symbol that can't be the name of any user-defined class
	No_type,     // If e : No_type, then no code is generated for e.
	SELF_TYPE,   // Special code is generated for new SELF_TYPE.
	self,        // self generates code differently than other references

	// extras
	arg,
	arg2,
	prim_string,
	prim_int,
	prim_bool;


//********************************************************
//
// PREDEFINED FUNCTIONS:
//
// The following functions are already coded, you should
// not need to modify them, although you may if necessary.
//
//********************************************************

//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
	Object      = idtable.add_string("Object");
	IO          = idtable.add_string("IO");
	String      = idtable.add_string("String");
	Int         = idtable.add_string("Int");
	Bool        = idtable.add_string("Bool");
	Main        = idtable.add_string("Main");

	cool_abort  = idtable.add_string("abort");
	type_name   = idtable.add_string("type_name");
	cool_copy   = idtable.add_string("copy");
	out_string  = idtable.add_string("out_string");
	out_int     = idtable.add_string("out_int");
	in_string   = idtable.add_string("in_string");
	in_int      = idtable.add_string("in_int");
	length      = idtable.add_string("length");
	concat      = idtable.add_string("concat");
	substr      = idtable.add_string("substr");

	val         = idtable.add_string("val");

	No_class    = idtable.add_string("_no_class");
	No_type     = idtable.add_string("_no_type");
	SELF_TYPE   = idtable.add_string("SELF_TYPE");
	self        = idtable.add_string("self");

	arg         = idtable.add_string("arg");
	arg2        = idtable.add_string("arg2");
	prim_string = idtable.add_string("sbyte*");
	prim_int    = idtable.add_string("int");
	prim_bool   = idtable.add_string("bool");
}

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************
void program_class::cgen(ostream &os) 
{
	initialize_constants();
	class_table = new CgenClassTable(classes,os);
}


// Create definitions for all String constants
void StrTable::code_string_table(ostream& s, CgenClassTable* ct)
{
	for (List<StringEntry> *l = tbl; l; l = l->tl()) {
		l->hd()->code_def(s, ct);
	}
}

// Create definitions for all Int constants
void IntTable::code_string_table(ostream& s, CgenClassTable* ct)
{
	for (List<IntEntry> *l = tbl; l; l = l->tl()) {
		l->hd()->code_def(s, ct);
	}
}

//
// Sets up declarations for extra functions needed for code generation
// You should not need to modify this code for MP2.1
//
void CgenClassTable::setup_external_functions()
{
	ValuePrinter vp;
	// setup function: external int strcmp(sbyte*, sbyte*)
	op_type i32_type(INT32), i8ptr_type(INT8_PTR), vararg_type(VAR_ARG);
	vector<op_type> strcmp_args;
	strcmp_args.push_back(i8ptr_type);
	strcmp_args.push_back(i8ptr_type);	
	vp.declare(*ct_stream, i32_type, "strcmp", strcmp_args); 

	// setup function: external int printf(sbyte*, ...)
	vector<op_type> printf_args;
	printf_args.push_back(i8ptr_type);
	printf_args.push_back(vararg_type);
	vp.declare(*ct_stream, i32_type, "printf", printf_args);

	// setup function: external void abort(void)
	op_type void_type(VOID);
	vector<op_type> abort_args;
	vp.declare(*ct_stream, void_type, "abort", abort_args);

	// setup function: external i8* malloc(i32)
	vector<op_type> malloc_args;
	malloc_args.push_back(i32_type);
	vp.declare(*ct_stream, i8ptr_type, "malloc", malloc_args);

 #ifdef MP3  
	//ADD CODE HERE
	//Setup external functions for built in object class functions
	// declare pointer types for basic class objects
	op_type obj_ptr_type("Object", 1), int_ptr_type("Int", 1), io_ptr_type("IO", 1), bool_ptr_type("Bool", 1), str_ptr_type("String", 1); 
    // setup function: %Object* Object_new()
    vector<op_type> Object_new_args; 
    vp.declare(*ct_stream, obj_ptr_type, "Object_new", Object_new_args); 

    // setup function: %Object* Object_abort(%Object*)
    vector<op_type> Object_abort_args;
    Object_abort_args.push_back(obj_ptr_type); 
    vp.declare(*ct_stream, obj_ptr_type, "Object_abort", Object_abort_args); 

    // setup function: %String* Object_type_name(%Object*)
	vector<op_type> Object_type_name_args; 
    Object_type_name_args.push_back(obj_ptr_type);
    vp.declare(*ct_stream, str_ptr_type, "Object_type_name", Object_type_name_args); 
    
    // setup function: %Object* Object_copy(%Object*)
    vector<op_type> Object_copy_args; 
    Object_copy_args.push_back(obj_ptr_type);
    vp.declare(*ct_stream, obj_ptr_type, "Object_copy", Object_copy_args); 

    // setup function: %IO* IO_new()
    vector<op_type> IO_new_args; 
    vp.declare(*ct_stream, io_ptr_type, "IO_new", IO_new_args); 

    // setup function: %IO* IO_out_string(IO*, String*)
    vector<op_type> IO_out_string_args; 
    IO_out_string_args.push_back(io_ptr_type);
    IO_out_string_args.push_back(str_ptr_type); 
    vp.declare(*ct_stream, io_ptr_type, "IO_out_string", IO_out_string_args);

    // setup function: IO* IO_out_int(IO*, i32)
    vector<op_type> IO_out_int_args;
    IO_out_int_args.push_back(io_ptr_type);
    IO_out_int_args.push_back(i32_type); 
    vp.declare(*ct_stream, io_ptr_type, "IO_out_int", IO_out_int_args); 

    // setup function: String* IO_in_string(IO*)
    vector<op_type> IO_in_string_args;
    IO_in_string_args.push_back(io_ptr_type);
    vp.declare(*ct_stream, str_ptr_type, "IO_in_string", IO_in_string_args);

    // setup function: i32 IO_in_int(IO*)
    vector<op_type> IO_in_int_args; 
    IO_in_int_args.push_back(io_ptr_type);
    vp.declare(*ct_stream, i32_type, "IO_in_int", IO_in_int_args); 

    // setup function: String* String_new()
    vector<op_type> String_new_args; 
    vp.declare(*ct_stream, str_ptr_type, "String_new", String_new_args); 

    // setup function: i32 String_length(String*)
    vector<op_type> String_length_args; 
    String_length_args.push_back(str_ptr_type); 
    vp.declare(*ct_stream, i32_type, "String_length", String_length_args); 

    // setup function: String* String_concat(String*, String*)
    vector<op_type> String_concat_args; 
    String_concat_args.push_back(str_ptr_type);
    String_concat_args.push_back(str_ptr_type);
    vp.declare(*ct_stream, str_ptr_type, "String_concat", String_concat_args);

    // setup function: String* String_substr(String*, i32, i32)
    vector<op_type> String_substr_args;
    String_substr_args.push_back(str_ptr_type);
    String_substr_args.push_back(i32_type);
    String_substr_args.push_back(i32_type);
    vp.declare(*ct_stream, str_ptr_type, "String_substr", String_substr_args); 

    // setup function: Int* Int_new()
    vector<op_type> Int_new_args; 
    vp.declare(*ct_stream, int_ptr_type, "Int_new", Int_new_args);
   
    // setup function: void Int_init(Int*, i32)
    vector<op_type> Int_init_args;
    Int_init_args.push_back(int_ptr_type);
    Int_init_args.push_back(i32_type);
    vp.declare(*ct_stream, void_type, "Int_init", Int_init_args); 

    // setup function: Bool* Bool_new()
    vector<op_type> Bool_new_args;
    vp.declare(*ct_stream, bool_ptr_type, "Bool_new", Bool_new_args);
    
    // setup function: void Bool_init(Bool*, i1)
    vector<op_type> Bool_init_args; 
    Bool_init_args.push_back(bool_ptr_type);
    op_type i1_type(INT1); 
    Bool_init_args.push_back(i1_type); 
    vp.declare(*ct_stream, void_type, "Bool_init", Bool_init_args); 
#endif
}

// Creates AST nodes for the basic classes and installs them in the class list
void CgenClassTable::install_basic_classes()
{
	// The tree package uses these globals to annotate the classes built below.
	curr_lineno = 0;
	Symbol filename = stringtable.add_string("<basic class>");

	//
	// A few special class names are installed in the lookup table but not
	// the class list. Thus, these classes exist, but are not part of the
	// inheritance hierarchy.
	 
	// No_class serves as the parent of Object and the other special classes.
	Class_ noclasscls = class_(No_class,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(noclasscls, CgenNode::Basic, this));
	delete noclasscls;

#ifdef MP3
	// SELF_TYPE is the self class; it cannot be redefined or inherited.
	Class_ selftypecls = class_(SELF_TYPE,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(selftypecls, CgenNode::Basic, this));
	delete selftypecls;
	// 
	// Primitive types masquerading as classes. This is done so we can
	// get the necessary Symbols for the innards of String, Int, and Bool
	//
	Class_ primstringcls = class_(prim_string,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(primstringcls, CgenNode::Basic, this));
	delete primstringcls;
#endif
	Class_ primintcls = class_(prim_int,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(primintcls, CgenNode::Basic, this));
	delete primintcls;
	Class_ primboolcls = class_(prim_bool,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(primboolcls, CgenNode::Basic, this));
	delete primboolcls;
	// 
	// The Object class has no parent class. Its methods are
	//        cool_abort() : Object   aborts the program
	//        type_name() : Str       returns a string representation of class name
	//        copy() : SELF_TYPE      returns a copy of the object
	//
	// There is no need for method bodies in the basic classes---these
	// are already built in to the runtime system.
	//
	Class_ objcls =
		class_(Object, 
		       No_class,
		       append_Features(
		       append_Features(
		       single_Features(method(cool_abort, nil_Formals(), 
		                              Object, no_expr())),
		       single_Features(method(type_name, nil_Formals(),
		                              String, no_expr()))),
		       single_Features(method(cool_copy, nil_Formals(), 
		                              SELF_TYPE, no_expr()))),
		       filename);
	install_class(new CgenNode(objcls, CgenNode::Basic, this));
	delete objcls;

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
	Class_ intcls=
		class_(Int, 
		       Object,
		       single_Features(attr(val, prim_int, no_expr())),
		       filename);
	install_class(new CgenNode(intcls, CgenNode::Basic, this));
	delete intcls;

//
// Bool also has only the "val" slot.
//
	Class_ boolcls=
		class_(Bool,  
		       Object, 
		       single_Features(attr(val, prim_bool, no_expr())),
		       filename);
	install_class(new CgenNode(boolcls, CgenNode::Basic, this));
	delete boolcls;

#ifdef MP3
//
// The class String has a number of slots and operations:
//       val                                  the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
	Class_ stringcls =
		class_(String, 
		       Object,
		       append_Features(
		       append_Features(
		       append_Features(
		       single_Features(attr(val, prim_string, no_expr())),
		       single_Features(method(length, nil_Formals(),
		                              Int, no_expr()))),
		       single_Features(method(concat,
		                              single_Formals(formal(arg, String)),
		                              String,
		                              no_expr()))),
		       single_Features(method(substr, 
		                              append_Formals(
		                                 single_Formals(formal(arg, Int)), 
		                                 single_Formals(formal(arg2, Int))),
		                              String, 
		                              no_expr()))),
		       filename);
	install_class(new CgenNode(stringcls, CgenNode::Basic, this));
	delete stringcls;
#endif

#ifdef MP3
// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
	Class_ iocls =
		class_(IO,
		       Object,
		       append_Features(
		       append_Features(
		       append_Features(
		       single_Features(method(out_string,
		                              single_Formals(formal(arg, String)),
		                              SELF_TYPE, no_expr())),
		       single_Features(method(out_int, single_Formals(formal(arg, Int)),
		                              SELF_TYPE, no_expr()))),
		       single_Features(method(in_string, nil_Formals(), String,
		                              no_expr()))),
		       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
		       filename);
	install_class(new CgenNode(iocls, CgenNode::Basic, this));
	delete iocls;
#endif
}

//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_classes(Classes cs)
{
	for (int i = cs->first(); cs->more(i); i = cs->next(i)) {
		install_class(new CgenNode(cs->nth(i),CgenNode::NotBasic,this));
	}
}

// 
// Add this CgenNode to the class list and the lookup table
// 
void CgenClassTable::install_class(CgenNode *nd)
{
	Symbol name = nd->get_name(); // FIXME where is the get_name() method of CgenNode?

	if (probe(name))
		return;

	// The class name is legal, so add it to the list of classes
	// and the symbol table.
	nds = new List<CgenNode>(nd,nds);
	addid(name,nd);
}

// 
// Add this CgenNode to the special class list and the lookup table
// 
void CgenClassTable::install_special_class(CgenNode *nd)
{
	Symbol name = nd->get_name();

	if (probe(name))
		return;

	// The class name is legal, so add it to the list of special classes
	// and the symbol table.
	special_nds = new List<CgenNode>(nd, special_nds);
	addid(name,nd);
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
	for(List<CgenNode> *l = nds; l; l = l->tl())
		set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNode *nd)
{
	CgenNode *parent_node = probe(nd->get_parent());
	nd->set_parentnd(parent_node);
	parent_node->add_child(nd);
}

// Get the root of the class tree.
CgenNode *CgenClassTable::root()
{
	return probe(Object);
}

//////////////////////////////////////////////////////////////////////
//
// Special-case functions used for the method Int Main::main() for
// MP2-1 only.
//
//////////////////////////////////////////////////////////////////////

#ifndef MP3

CgenNode* CgenClassTable::getMainmain(CgenNode* c)
{
	if (c && ! c->basic())
		return c;                   // Found it!

	List<CgenNode> *children = c->get_children();
	for (List<CgenNode> *child = children; child; child = child->tl()) {
		if (CgenNode* foundMain = this->getMainmain(child->hd()))
			return foundMain;   // Propagate it up the recursive calls
	}

	return 0;                           // Make the recursion continue
}

#endif

//-------------------------------------------------------------------
//
// END OF PREDEFINED FUNCTIONS
//
//-------------------------------------------------------------------


///////////////////////////////////////////////////////////////////////////////
//
// coding string, int, and boolean constants
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type stringEntry.  stringEntry methods are defined both for string
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Create global definitions for constant Cool objects
//
void CgenClassTable::code_constants()
{
#ifdef MP3

	// ADD CODE HERE
#endif
}

// generate code to define a global string constant
void StringEntry::code_def(ostream& s, CgenClassTable* ct)
{
#ifdef MP3
	// ADD CODE HERE
#endif
}

// generate code to define a global int constant
void IntEntry::code_def(ostream& s, CgenClassTable* ct)
{
	// Leave this method blank, since we are not going to use global
	// declarations for int constants.
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//
// CgenClassTable constructor orchestrates all code generation
//
CgenClassTable::CgenClassTable(Classes classes, ostream& s) 
: nds(0)
{
	if (cgen_debug) std::cerr << "Building CgenClassTable" << endl;
	ct_stream = &s;
	// Make sure we have a scope, both for classes and for constants
	enterscope();

	// Create an inheritance tree with one CgenNode per class.
	install_basic_classes();
	install_classes(classes);
	build_inheritance_tree();

	// First pass
	setup();

	// Second pass
	code_module();
	// Done with code generation: exit scopes
	exitscope();

}

CgenClassTable::~CgenClassTable()
{
}

// The code generation first pass.  Define these two functions to traverse
// the tree and setup each CgenNode
void CgenClassTable::setup()
{
	setup_external_functions();
	setup_classes(root(), 0);
}


void CgenClassTable::setup_classes(CgenNode *c, int depth)
{
	// MAY ADD CODE HERE
	// if you want to give classes more setup information

	c->setup(current_tag++, depth); // FIXME what to do with depth? 
	List<CgenNode> *children = c->get_children();
	for (List<CgenNode> *child = children; child; child = child->tl())
		setup_classes(child->hd(), depth + 1);
	
	c->set_max_child(current_tag-1);

	/*
	if (cgen_debug)
		std::cerr << "Class " << c->get_name() << " assigned tag " 
			<< c->get_tag() << ", max child " << c->get_max_child() 
			<< ", depth " << c->get_depth() << endl;
	*/
}


// The code generation second pass. Add code here to traverse the tree and
// emit code for each CgenNode
void CgenClassTable::code_module()
{
	code_constants();

#ifndef MP3
	// This must be after code_module() since that emits constants
	// needed by the code() method for expressions
	CgenNode* mainNode = getMainmain(root());
	mainNode->codeGenMainmain();
#endif
	code_main();

#ifdef MP3
	code_classes(root());
#else
#endif
}


#ifdef MP3
void CgenClassTable::code_classes(CgenNode *c)
{

	// ADD CODE HERE
	if (c->basic()) return; 

}
#endif


//
// Create LLVM entry point. This function will initiate our Cool program 
// by generating the code to execute (new Main).main()
//
void CgenClassTable::code_main()
{
    // CgenEnvironment cenv(*ct_stream, root()); // in order to use new_name()

    op_type i32_type(INT32), i8_type(INT8), i8ptr_type(INT8_PTR), vararg_type(VAR_ARG);
    ValuePrinter vp(*ct_stream);

    // Define the printout string of main function
    op_arr_type str_type(INT8, 25);
    op_arr_ptr_type str_ptr_type(INT8, 25);
    const_value val_const(str_type, "Main.main() returned %d\n", 1);
    global_value val_g(str_ptr_type, "main.printout.str", val_const);
    vp.init_constant(*ct_stream, "main.printout.str", val_const); // lacking align 1 

	// Define a function main that has no parameters and returns an i32
    vector<operand> main_args;
    vp.define(*ct_stream, i32_type, "main", main_args);
    
	// Define an entry basic block
    vp.begin_block("entry");

	// Call Main.main(). This returns int* for phase 1, Object for phase 2
    vector<op_type> mainmain_args_types;
    vector<operand> mainmain_args;
    operand ret = vp.call(mainmain_args_types, i32_type, "Main_main", 1, mainmain_args);
    
    
#ifndef MP3
	// Get the address of the string "Main_main() returned %d\n" using
	// getelementptr
    int_value zero(0);
    operand str_ptr(i8ptr_type, "tpm"); // to coform the reference ll 
    vp.getelementptr(*ct_stream, str_type, val_g, zero, zero, str_ptr);

	// Call printf with the string address of "Main.main() returned %d\n"
	// and the return value of Main_main() as its arguments
    vector<op_type> printf_args_types;
    printf_args_types.push_back(i8ptr_type);
    printf_args_types.push_back(vararg_type);
    vector<operand> printf_args;
    printf_args.push_back(str_ptr);
    printf_args.push_back(ret);
    operand ret_printf = vp.call(printf_args_types, i32_type, "printf", 1, printf_args);

	// Insert return 0
    vp.ret(zero);
    vp.end_define();

#else
	// Phase 2
#endif

}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTable *ct)
: class__class((const class__class &) *nd), 
  parentnd(0), children(0), basic_status(bstatus), class_table(ct), tag(-1)
{ 
	// ADD CODE HERE
	// similar to CgenClassTable, call functions to generate code for a class
	mtd_tbl.enterscope();
    attr_tbl.enterscope();
    
    attr_index = 1; // account for vtable
    mtd_index = 4;  // account for tag, ptrtoint, getelementptr, new 
}

void CgenNode::add_child(CgenNode *n)
{
	children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNode *p)
{
	assert(parentnd == NULL);
	assert(p != NULL);
	parentnd = p;
}

//
// Class setup.  You may need to add parameters to this function so that
// the classtable can provide setup information (such as the class tag
// that should be used by this class).  
// 
// Things that setup should do:
//  - layout the features of the class
//  - create the types for the class and its vtable
//  - create global definitions used by the class such as the class vtable
//
void CgenNode::setup(int tag, int depth)
{
	this->tag = tag;
#ifdef MP3
	layout_features();

	// ADD CODE HERE
	ValuePrinter vp; 
        
    vp.type_define(*(this->class_table->ct_stream), this->get_type_name(), cls_record);
    vp.type_define(*(this->class_table->ct_stream), this->get_type_name()+"_vtable", vtable); 
    global_value prototype(op_type(this->get_type_name()+"_vtable"), this->get_type_name()+"_vtable_prototype"); 
    vp.init_struct_constant(*(this->class_table->ct_stream), prototype, vtable, vtable_prototype); 
#endif
}

#ifdef MP3
//
// Class codegen. This should performed after every class has been setup.
// Generate code for each method of the class.
//
void CgenNode::code_class()
{
	// No code generation for basic classes. The runtime will handle that.
	if (basic())
		return;
	
	// ADD CODE HERE
}

// Laying out the features involves creating a Function for each method
// and assigning each attribute a slot in the class structure.
void CgenNode::layout_features()
{
	// ADD CODE HERE
	// create str.Class constant
	ValuePrinter vp; 
    op_arr_type str_type(INT8, this->get_type_name().length()+1);
    // op_arr_ptr_type str_ptr_type(INT8, this->get_type_name().length()+1);
    const_value val_const(str_type, this->get_type_name(), 1);
    // cerr << val_const.get_value() << endl;
    // cerr << val_const.get_typename() << endl; 
    // cerr << val_const.get_type().get_id() << endl; 
    // global_value val_g(str_ptr_type, "str."+this->get_type_name(), val_const); 
    vp.init_constant(*(this->class_table->ct_stream), "str."+this->get_type_name(), val_const); // lacking align 1	
    cerr << "after init_constant" << endl; // FIXME    

    op_type i32_type(INT32), i8_ptr_type(INT8_PTR), obj_ptr_type("Object", 1);  
    // handle Object class
    if (this->get_type_name() == std::string("Object")) {
        cls_record.push_back(op_type("Object_vtable", 1)); 
        vtable.push_back(i32_type);
        vtable_prototype.push_back(int_value(tag));
        vtable_ptr.push_back(NULL); // no need to save pointers to not casted_value type
        vtable.push_back(i32_type);
        vtable_prototype.push_back(const_value(i32_type,"ptrtoint (%"+get_type_name()+"* getelementptr (%"+get_type_name()+", %"+get_type_name()+"* null, i32 1) to i32)", 1));
        vtable_ptr.push_back(NULL);
        vtable.push_back(i8_ptr_type);
        const_value str_Class(str_type, "@str."+this->get_type_name(), 1); 
        vtable_prototype.push_back(str_Class); 
        vtable_ptr.push_back(NULL);
        // add Object_new in vtable
        vector<op_type> obj_new_args; 
        op_func_type obj_new(obj_ptr_type, obj_new_args);
        vtable.push_back(obj_new);
        vtable_prototype.push_back(const_value(op_type(get_type_name(),1), "@"+get_type_name()+"_new", 1)); 
        vtable_ptr.push_back(NULL);
        list_node<Feature> *obj_fl = features;
        for (int j = obj_fl->first(); obj_fl->more(j); j = obj_fl->next(j)) {
            obj_fl->nth(j)->layout_feature(this); 
        }
        
    } else {
        // copy the class record of the parent while change the name of vtable
        // cls_record = parentnd->cls_record;
        // cls_record.erase(cls_record.begin());
        cls_record.push_back(op_type(this->get_type_name()+std::string("_vtable"), 1));
        // copy the vtable of the parent class
        vtable.push_back(i32_type);
        vtable_prototype.push_back(int_value(tag));
        vtable_ptr.push_back(NULL);
        vtable.push_back(i32_type);
        vtable_prototype.push_back(const_value(i32_type,"ptrtoint (%"+get_type_name()+"* getelementptr (%"+get_type_name()+", %"+get_type_name()+"* null, i32 1) to i32)", 1));  
        vtable_ptr.push_back(NULL);  
        vtable.push_back(i8_ptr_type);
        const_value str_Class(str_type, "@str."+this->get_type_name(), 1); 
        vtable_prototype.push_back(str_Class);
        vtable_ptr.push_back(NULL); 
        // add Class_new in the vtable
        vector<op_type> new_args; 
        op_func_type cls_new(op_type(get_type_name(), 1), new_args);
        vtable.push_back(cls_new);
        vtable_prototype.push_back(const_value(op_type(get_type_name(),1), "@"+get_type_name()+"_new", 1));
        vtable_ptr.push_back(NULL); 
        // go through the parents' feature list and add appropriate method signature
        list_node<Feature> *pfl = nil_Features();
        for (CgenNode *p = parentnd; p->get_type_name().compare("_no_class") != 0; p = p->parentnd) {
            
            pfl = pfl->append(p->features, pfl);
            // pfl->dump(cout, 2); 
        }
        for (int k = pfl->first(); pfl->more(k); k = pfl->next(k)) {
            pfl->nth(k)->layout_feature(this);
        }        
        // go through the feature list, add attributes and methods into cls_record or vtable
	    list_node<Feature> *fl = features;
        // fl->dump(cout,2); 
        for (int i = fl->first(); fl->more(i); i = fl->next(i)) {
            fl->nth(i)->layout_feature(this);
        }
    }
    mtd_tbl.dump(); 
    attr_tbl.dump();  
}
#else

// 
// code-gen function main() in class Main
//
void CgenNode::codeGenMainmain()
{
	ValuePrinter vp;
	// In Phase 1, this can only be class Main. Get method_class for main().
	assert(std::string(this->name->get_string()) == std::string("Main"));
	method_class* mainMethod = (method_class*) features->nth(features->first());

	// ADD CODE HERE TO GENERATE THE FUNCTION int Mainmain().
	// Generally what you need to do are:
	// -- setup or create the environment, env, for translating this method
	// -- invoke mainMethod->code(env) to translate the method
	CgenEnvironment mainMethodEnv(*(this->class_table->ct_stream), this);
    mainMethod->code(&mainMethodEnv); 
	
}

#endif

//
// CgenEnvironment functions
//

//
// Class CgenEnvironment should be constructed by a class prior to code
// generation for each method.  You may need to add parameters to this
// constructor.
//
CgenEnvironment::CgenEnvironment(std::ostream &o, CgenNode *c)
{
	cur_class = c;
	cur_stream = &o;
	var_table.enterscope();
	tmp_count = block_count = ok_count = 0;
	// ADD CODE HERE
}

// Look up a CgenNode given a symbol
CgenNode *CgenEnvironment::type_to_class(Symbol t) {
	return t == SELF_TYPE ? get_class() 
		: get_class()->get_classtable()->lookup(t);
}

// Provided CgenEnvironment methods
// Generate unique string names
std::string CgenEnvironment::new_name() {
	std::stringstream s;
	s << tmp_count++;
	return "tmp." + s.str();
}

std::string CgenEnvironment::new_ok_label() {
	std::stringstream s;
	s << ok_count++;
	return "ok." + s.str();
}
const std::string CgenEnvironment::new_label(const std::string& prefix,
		bool increment) {
	std::string suffix = itos(block_count);
	block_count += increment;
	return prefix + "." + suffix; // Add '.'
}

void CgenEnvironment::add_local(Symbol name, operand &vb) {
	var_table.enterscope();
	var_table.addid(name, &vb);
}

void CgenEnvironment::kill_local() {
	var_table.exitscope();
}


////////////////////////////////////////////////////////////////////////////
//
// APS class methods
//
////////////////////////////////////////////////////////////////////////////

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.handcode.h'.
//
//*****************************************************************

#ifdef MP3
// conform and get_class_tag are only needed for MP3

// conform - If necessary, emit a bitcast or boxing/unboxing operations
// to convert an object to a new type. This can assume the object
// is known to be (dynamically) compatible with the target type.
// It should only be called when this condition holds.
// (It's needed by the supplied code for typecase)
operand conform(operand src, op_type type, CgenEnvironment *env) {
	// ADD CODE HERE (MP3 ONLY)
	return operand();
}

// Retrieve the class tag from an object record.
// src is the object we need the tag from.
// src_class is the CgenNode for the *static* class of the expression.
// You need to look up and return the class tag for it's dynamic value
operand get_class_tag(operand src, CgenNode *src_cls, CgenEnvironment *env) {
	// ADD CODE HERE (MP3 ONLY)
	return operand();
}
#endif

//
// Create a method body
// 
void method_class::code(CgenEnvironment *env)
{
	if (cgen_debug) std::cerr << "method" << endl;
    assert(return_type == Int); // for MP2, the return type must be Int
    ValuePrinter vp(*(env->cur_stream));
	
	// MP2 supports main method only 
	// Construct the signature first
	// Set up return type ADD CODE for MP3 
    op_type ret_type; 
    op_type i32_type(INT32); // Only support Int for MP2
    if (return_type == Int) ret_type.set_type(i32_type);
    // Construct the argument list 
    // For MP2 it is empty for the Main_main method 
    vector<operand> main_args;
    vp.define(ret_type, "Main_main", main_args); 
    // Create a new basic block entry
    vp.begin_block("entry"); 
    // Code generation for expression
    operand ret_value = expr->code(env);
    // Return 
    vp.ret(ret_value); 
    // Construct abort block to finish the method or handle errors
    vp.begin_block("abort"); 
    vector<operand> abort_args;
    vector<op_type> abort_args_type;
    operand result_abort(op_type(VOID), ""); 
    vp.call(*(env->cur_stream), abort_args_type, "abort", 1, abort_args, result_abort);
    vp.unreachable(); 

    vp.end_define(); 
}

//
// Codegen for expressions.  Note that each expression has a value.
//

operand assign_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "assign" << endl;
	ValuePrinter vp;
	operand val = expr->code(env);
    operand ptr = *(env->lookup(name));
    vp.store(*(env->cur_stream), val, ptr);

    return val;  
}

operand cond_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "cond" << endl;
	ValuePrinter vp(*(env->cur_stream)); 
	operand ret_ptr; 
    op_type ret_type; 
    // MP2 Assume both branches are either Int or Bool
    if (then_exp->get_type() == Int) { 
        ret_ptr = operand(op_type(INT32_PTR), env->new_name()); 
        ret_type.set_type(op_type(INT32)); 
    } else {
        ret_ptr = operand(op_type(INT1_PTR), env->new_name()); 
        ret_type.set_type(op_type(INT1)); 
    }
    // Allocate stack for return value
    vp.alloca_mem(*(env->cur_stream), ret_type, ret_ptr);  
    // Generate code for predictive
    operand pred_op = pred->code(env);
    // Create label for 'then', 'else', and 'end' block
    string then_label = env->new_label("true", false);
    string else_label = env->new_label("false", false);
    string end_label = env->new_label("end", true);  
    // Branch conditionally to 'then' or 'else' block
    vp.branch_cond(pred_op, then_label, else_label);
    // Construct 'then' block
    vp.begin_block(then_label); 
    operand then_op = then_exp->code(env); 
    vp.store(then_op, ret_ptr);
    vp.branch_uncond(end_label);  
    // Construct 'else' block 
    vp.begin_block(else_label); 
    operand else_op = else_exp->code(env); 
    vp.store(else_op, ret_ptr);
    vp.branch_uncond(end_label); 
    // Start 'end' block
    vp.begin_block(end_label);
    // Load back and return the return value 
    operand ret_val(ret_type, env->new_name()); 
    vp.load(*(env->cur_stream), ret_type, ret_ptr, ret_val);
    return ret_val; 
}

operand loop_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "loop" << endl;
	ValuePrinter vp(*(env->cur_stream)); 
    // Create the predictive label, true block label, and false block label
    // Use the same number surfix for a loop or a if-then-else
    string pred_label = env->new_label("loop", false); 
    string true_label = env->new_label("true", false);
    string false_label = env->new_label("false", true);     
    // Jump to the predictive evaluation block
    vp.branch_uncond(pred_label);
    vp.begin_block(pred_label);    
    // Evaluate the predictive
	operand pred_op = pred->code(env); 
    // Conditionally jump to true or false label
    vp.branch_cond(pred_op, true_label, false_label);  
    // Construct the true block
    vp.begin_block(true_label); 
    body->code(env); 
    vp.branch_uncond(pred_label); 
    // Construct the false block
    vp.begin_block(false_label);
    
    return int_value(0); // for MP2  
} 

operand block_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "block" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
	operand ret_value; 
	for (int i = body->first(); body->more(i); i = body->next(i)) {
        ret_value = body->nth(i)->code(env);
    }
	return ret_value;
}

operand let_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "let" << endl;
	ValuePrinter vp; 
    operand init_op = init->code(env);
    // Code generation for the 'let' part
    op_type init_type;
    // Check for uninitialized let bindings 
    // Should work for only Int and Bool, think more when Object is added
    if (init_op.is_empty()) {
        if (type_decl == Int) {
            init_type.set_type(op_type(INT32));
            init_op = int_value(0); // default initialization to 0
        } else if (type_decl == Bool) {
            init_type.set_type(op_type(INT1));
            init_op = bool_value(0, 1); // default initialization to false  
        } else {
           assert(0 && "Unsupported case for MP2");
        }
    } else {
        init_type = init_op.get_type();
    }
    // Create new pointer operand, allocate on stack, and store initial value
    operand ptr(init_type.get_ptr_type(), env->new_name());
    vp.alloca_mem(*(env->cur_stream), init_type, ptr); 
    vp.store(*(env->cur_stream), init_op, ptr);
    // Add local variable to the var_table
    env->add_local(identifier, ptr);
    // Code generation for the body
    operand ret = body->code(env);
    // Jump out of one scope
    env->kill_local();
    
    return ret;   
}

operand plus_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "plus" << endl;
    ValuePrinter vp;
    operand op1 = e1->code(env);
    operand op2 = e2->code(env);
    operand result(op1.get_type(), env->new_name()); 
    vp.add(*(env->cur_stream), op1, op2, result);
    return result; 
}

operand sub_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "sub" << endl;
	ValuePrinter vp;
    operand op1 = e1->code(env);
    operand op2 = e2->code(env);
    operand result(op1.get_type(), env->new_name()); 
	vp.sub(*(env->cur_stream), op1, op2, result);
    return result;
}

operand mul_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "mul" << endl;
	ValuePrinter vp;
    operand op1 = e1->code(env);
    operand op2 = e2->code(env);
    operand result(op1.get_type(), env->new_name()); 
	vp.mul(*(env->cur_stream), op1, op2, result);
    return result;
}

operand divide_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "div" << endl;
	ValuePrinter vp(*(env->cur_stream));
    operand op1 = e1->code(env);
    operand op2 = e2->code(env);
    // Create a new ok label
    string ok_label = env->new_ok_label(); 
    // Check dividing by 0 
    operand div0(op_type(INT1), env->new_name()); 
    vp.icmp(*(env->cur_stream), EQ, op2, int_value(0), div0); 
    vp.branch_cond(*(env->cur_stream), div0, "abort", ok_label);
    // If no dividing by 0
    vp.begin_block(ok_label);  
    operand result(op1.get_type(), env->new_name()); 
	vp.div(*(env->cur_stream), op1, op2, result);
    return result;
}

operand neg_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "neg" << endl;
	// ~Int (-n = 0-n)
    ValuePrinter vp;
    int_value op1(0);
    operand op2 = e1->code(env);
    operand result(op2.get_type(), env->new_name());
    vp.sub(*(env->cur_stream), op1, op2, result);
    return result; 
}

operand lt_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "lt" << endl;
    ValuePrinter vp;
    operand op1 = e1->code(env);
    operand op2 = e2->code(env);
    // result must be Bool
    operand result(op_type(INT1), env->new_name()); 
	vp.icmp(*(env->cur_stream), LT, op1, op2, result);
    return result;
}

operand eq_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "eq" << endl;
	ValuePrinter vp;
    operand op1 = e1->code(env);
    operand op2 = e2->code(env);
    // result must be Bool
    operand result(op_type(INT1), env->new_name()); 
	vp.icmp(*(env->cur_stream), EQ, op1, op2, result);
    return result;
}

operand leq_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "leq" << endl;
	ValuePrinter vp;
    operand op1 = e1->code(env);
    operand op2 = e2->code(env);
    // result must be Bool
    operand result(op_type(INT1), env->new_name()); 
	vp.icmp(*(env->cur_stream), LE, op1, op2, result);
    return result;
}

operand comp_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "complement" << endl;
    // not Bool 
	ValuePrinter vp;
    operand op1 = e1->code(env);
    operand result(op1.get_type(), env->new_name());
    // Use xor to revert the Bool value 
    vp.xor_in(*(env->cur_stream), op1, bool_value(1, 1), result);
    return result; 
}

operand int_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "Integer Constant" << endl;
	return int_value(atoi(token->get_string()));
}

operand bool_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "Boolean Constant" << endl;
	return bool_value(val, 1);  
}

operand object_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "Object" << endl;
    ValuePrinter vp;
    // Check symbol table for the object name
    operand ptr = *(env->lookup(name));
    // Load value for use
    op_type val_type = ptr.get_type().get_deref_type();
    operand val(val_type, env->new_name());
    vp.load(*(env->cur_stream), val_type, ptr, val);
    return val; 
}

operand no_expr_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "No_expr" << endl;
	// Leave it as it is for MP2, handle it in 'let'
	return operand();
}

//*****************************************************************
// The next few functions are for node types not supported in Phase 1
// but these functions must be defined because they are declared as
// methods via the Expression_SHARED_EXTRAS hack.
//*****************************************************************

operand static_dispatch_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "static dispatch" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand string_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "string_const" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand dispatch_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "dispatch" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand typcase_class::code(CgenEnvironment *env)
{
	if (cgen_debug) 
		std::cerr << "typecase::code()" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand new__class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "newClass" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

operand isvoid_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "isvoid" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}
/*
op_type interpret_type(Symbol type) {
    if (type == Int) 
        return op_type(INT32);
    else if (type == Bool) 
        return op_type(INT1);
    else return op_type(type->get_string(), 1);
}
*/
 
// Create the LLVM Function corresponding to this method.

void method_class::layout_feature(CgenNode *cls) 
{
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE
    	
    // go through the formal list and form a vector of formal types
    vector<op_type> args;
    args.push_back(op_type(cls->get_type_name(), 1)); 
    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
            Symbol type = ((formal_class *)formals->nth(i))->get_type();
            if (type == Int) 
                args.push_back(op_type(INT32));
            else if (type == Bool)
                args.push_back(op_type(INT1));
            else
                args.push_back(op_type(type->get_string(), 1));
    }

    // get the return type and put a function type pointer in vtable
    op_type ret; 
    if (return_type == Int) 
        ret.set_type(op_type(INT32));
    else if (return_type == Bool)
        ret.set_type(op_type(INT1));  
    else if (return_type == SELF_TYPE)
        ret.set_type(op_type(cls->get_type_name(), 1));  
    else
        ret.set_type(op_type(return_type->get_string(), 1)); 

    op_func_type t(ret, args); 

    // check for inherited method 
    // if a method is already in vtable, then we know it is inherited and will be overwritten
    if (cls->lookup_mtd(name) != NULL) {
        // detect its entry in vtable_prototype (in bitcast form) and reinsert it in normal form
        cls->vtable_prototype.erase(cls->vtable_prototype.begin()+*(cls->lookup_mtd(name)));
        cls->vtable_prototype.insert(cls->vtable_prototype.begin()+*(cls->lookup_mtd(name)), const_value(ret, "@"+cls->get_type_name()+"_"+name->get_string(), 1)); 
    } else { // if a method is not found in vtable
        int *index = new int(cls->get_mtd_index()); // FIXME when to delete?  
        cls->add_mtd(name, index);
        cls->incre_mtd_index();
        cls->vtable.push_back(t); 
        // check if it is a method of the paretn class
        // if yes
        string precast_name; 
        if (cls->get_parentnd()->lookup_mtd(name) != NULL) {
            CgenNode *p = cls->get_parentnd();
            cerr << cls->get_type_name() << endl;
            op_type precast_type; 
            if ((p->get_type_name().compare("Object") == 0) || p->get_parentnd()->lookup_mtd(name) == NULL) {
                precast_type.set_type(p->vtable.at(*(p->lookup_mtd(name)))); 
                precast_name = "@"+p->get_type_name()+"_"+name->get_string(); 
            } else {
                 cerr << cls->get_type_name() << endl;
                cerr << name->get_string() << endl; 
                 p->get_parentnd()->dump_mtd_tbl();
                p->dump_vtable_ptr(); 
                cerr << "after" << endl; 
                 precast_type = p->vtable_ptr.at(*(p->lookup_mtd(name)))->get_precast_type(); // FIXME
                 string precast_ret = p->vtable_ptr.at(*(p->lookup_mtd(name)))->get_typename();
                 string precast_val = p->vtable_ptr.at(*(p->lookup_mtd(name)))->get_value(); 
                 precast_name = precast_val.substr(10+precast_type.get_name().length(), precast_val.length()-15-precast_type.get_name().length()-precast_ret.length()); 
            }
            // vector<op_type> precast_args = args; 
            // precast_args.erase(precast_args.begin()); 
            // precast_args.insert(precast_args.begin(), op_type(p->get_type_name(), 1)); 
            // op_func_type precast_type(ret,
            casted_value *casted_ptr = new casted_value(t, precast_name, precast_type);
            cls->vtable_prototype.push_back(*(casted_ptr));
            cls->vtable_ptr.push_back(casted_ptr);  
        } else { // if no
            cls->vtable_prototype.push_back(const_value(ret, "@"+cls->get_type_name()+"_"+name->get_string(), 1));
            cls->vtable_ptr.push_back(NULL);  
        }
    } 

#endif
}

// If the source tag is >= the branch tag and <= (max child of the branch class) tag,
// then the branch is a superclass of the source
operand branch_class::code(operand expr_val, operand tag,
				op_type join_type, CgenEnvironment *env) {
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
#endif
	return operand();
}

// Assign this attribute a slot in the class structure
void attr_class::layout_feature(CgenNode *cls)
{
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE
	// check for inherited attributes
	if (cls->lookup_attr(name) != NULL) { return; }
    else {
        int *index = new int(cls->get_attr_index()); 
        cls->add_attr(name, index);
        cls->incre_attr_index();
    }

    if (type_decl == prim_int || type_decl == Int) 
        cls->cls_record.push_back(op_type(INT32));
    else if (type_decl == prim_bool || type_decl == Bool) 
        cls->cls_record.push_back(op_type(INT1));
    else if (type_decl == SELF_TYPE)
        cls->cls_record.push_back(op_type(cls->get_type_name(), 1));
    else if (type_decl == prim_string)
        cls->cls_record.push_back(op_type(INT8_PTR)); 
    else
        cls->cls_record.push_back(op_type(type_decl->get_string(), 1));     
     
#endif
}

void attr_class::code(CgenEnvironment *env)
{
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE
#endif
}

