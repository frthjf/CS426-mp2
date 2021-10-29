/* 
 * This file provides the runtime library for cool. It implements
 * the cool classes in C.  Feel free to change it to match the structure
 * of your code generator.
*/

#include <stdbool.h>

typedef struct Object Object;
typedef struct String String;
typedef struct IO IO;
typedef struct Int Int;
typedef struct Bool Bool;

typedef struct Object_vtable Object_vtable;
typedef struct String_vtable String_vtable;
typedef struct IO_vtable IO_vtable;
typedef struct Int_vtable Int_vtable;
typedef struct Bool_vtable Bool_vtable;

/* class type definitions */
struct Object {
	/* ADD CODE HERE */
    const Object_vtable *vtblptr; 
};

struct Int {
	/* ADD CODE HERE */
    const Int_vtable *vtblptr; 
    int val; 
};

struct Bool {
	/* ADD CODE HERE */
    const Bool_vtable * vtblptr;
    bool val; 
};

struct String {
	/* ADD CODE HERE */
    const String_vtable *vtblptr; 
    char *val; 
};

struct IO {
	/* ADD CODE HERE */
    const IO_vtable *vtblptr; 
};


/* vtable type definitions */
struct Object_vtable {
	/* ADD CODE HERE */
    int tag; 
    int size; 
    const char *name; 
    Object* (*Object_new) (void);
    Object* (*Object_abort) (Object* obj);
    const String* (*Object_type_name) (Object* obj);
    Object* (*Object_copy) (Object* obj);
};

struct IO_vtable {
	/* ADD CODE HERE */
    int tag; 
    int size; 
    const char *name; 
    IO* (*IO_new) (void);
    Object* (*Object_abort) (IO* io);
    String* (*Object_type_name) (IO* io);
    IO* (*Object_copy) (IO* io);
    IO* (*IO_out_string) (IO* io, String* str); 
    IO* (*IO_out_int) (IO* io, int i); 
    String* (*IO_in_string) (IO* io); 
    int (*IO_in_int) (IO* io);
};

struct Int_vtable {
	/* ADD CODE HERE */
    int tag; 
    int size; 
    const char *name; 
    Int* (*Int_new) (void);
    Object* (*Object_abort) (Int* i);
    String* (*Object_type_name) (Int* i);
    Int* (*Object_copy) (Int* i);
};

struct Bool_vtable {
	/* ADD CODE HERE */
    int tag;
    int size;
    const char *name;
    Bool* (*Bool_new) (void);
    Object* (*Object_abort) (Bool* b);
    String* (*Object_type_name) (Bool* b);
    Bool* (*Object_copy) (Bool* b); 
};
   
struct String_vtable {
	/* ADD CODE HERE */
    int tag;
    int size;
    const char *name;
    String* (*String_new) (void);
    Object* (*Object_abort) (String *self);
    String* (*Object_type_name) (String *self );
    String* (*Object_copy) (String *self); 
    int (*String_length) (String *self);
    String* (*String_concat) (String *self, String *other);
    String* (*String_substr) (String *self, int start, int end); 
};

/* methods in class Object */
Object* Object_abort(Object *self);
const String* Object_type_name(Object *self);
	/* ADD CODE HERE */
Object* Object_new(void);
Object* Object_copy(Object *self); 

/* methods in class IO */
IO* IO_new(void);
void IO_init(IO *self); // FIXME not appear in the ll file??
IO* IO_out_string(IO *self, String *x);
IO* IO_out_int(IO *self, int x);
String* IO_in_string(IO *self);
int IO_in_int(IO *self);

/* methods in class Int */
	/* ADD CODE HERE */
Int* Int_new(void);
void Int_init(Int* self, int val);

/* methods in class Bool */
	/* ADD CODE HERE */
Bool* Bool_new(void);
void Bool_init(Bool* self, bool val);

/* methods in class String */
	/* ADD CODE HERE */
String* String_new(void);
int String_length(String *self);
String* String_concat(String *self, String *other);
String* String_substr(String *self, int start, int len); 
