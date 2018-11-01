#ifndef __org-w3c-dom_hh__
#define __org-w3c-dom_hh__

#include <omniORB2/CORBA.h>

#include <DOMString.hh>
#include <DOMException.hh>
#include <Node.hh>
#include <NodeList.hh>
#include <NamedNodeMap.hh>
#include <CharacterData.hh>
#include <DOMImplementation.hh>
#include <DocumentFragment.hh>
#include <Document.hh>
#include <Attr.hh>
#include <Element.hh>
#include <Text.hh>
#include <Comment.hh>
#include <CDATASection.hh>
#include <DocumentType.hh>
#include <Notation.hh>
#include <Entity.hh>
#include <EntityReference.hh>
#include <ProcessingInstruction.hh>
_CORBA_MODULE org_w3c_dom {
_CORBA_MODULE_PUBLIC

#ifndef __org_w3c_dom_DOMImplementation__
#define __org_w3c_dom_DOMImplementation__
  class   DOMImplementation;
  typedef DOMImplementation* DOMImplementation_ptr;
  typedef DOMImplementation_ptr DOMImplementationRef;
  class _proxy_DOMImplementation;
  class _sk_DOMImplementation;
  class _nil_DOMImplementation;

  class DOMImplementation_Helper {
    public:
    static DOMImplementation_ptr _nil();
    static CORBA::Boolean is_nil(DOMImplementation_ptr p);
    static void release(DOMImplementation_ptr p);
    static void duplicate(DOMImplementation_ptr p);
    static size_t NP_alignedSize(DOMImplementation_ptr obj,size_t initialoffset);
    static void marshalObjRef(DOMImplementation_ptr obj,NetBufferedStream &s);
    static DOMImplementation_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(DOMImplementation_ptr obj,MemBufferedStream &s);
    static DOMImplementation_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<DOMImplementation,DOMImplementation_Helper> DOMImplementation_var;

#endif
#ifndef __org_w3c_dom_DocumentFragment__
#define __org_w3c_dom_DocumentFragment__
  class   DocumentFragment;
  typedef DocumentFragment* DocumentFragment_ptr;
  typedef DocumentFragment_ptr DocumentFragmentRef;
  class _proxy_DocumentFragment;
  class _sk_DocumentFragment;
  class _nil_DocumentFragment;

  class DocumentFragment_Helper {
    public:
    static DocumentFragment_ptr _nil();
    static CORBA::Boolean is_nil(DocumentFragment_ptr p);
    static void release(DocumentFragment_ptr p);
    static void duplicate(DocumentFragment_ptr p);
    static size_t NP_alignedSize(DocumentFragment_ptr obj,size_t initialoffset);
    static void marshalObjRef(DocumentFragment_ptr obj,NetBufferedStream &s);
    static DocumentFragment_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(DocumentFragment_ptr obj,MemBufferedStream &s);
    static DocumentFragment_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<DocumentFragment,DocumentFragment_Helper> DocumentFragment_var;

#endif
#ifndef __org_w3c_dom_Document__
#define __org_w3c_dom_Document__
  class   Document;
  typedef Document* Document_ptr;
  typedef Document_ptr DocumentRef;
  class _proxy_Document;
  class _sk_Document;
  class _nil_Document;

  class Document_Helper {
    public:
    static Document_ptr _nil();
    static CORBA::Boolean is_nil(Document_ptr p);
    static void release(Document_ptr p);
    static void duplicate(Document_ptr p);
    static size_t NP_alignedSize(Document_ptr obj,size_t initialoffset);
    static void marshalObjRef(Document_ptr obj,NetBufferedStream &s);
    static Document_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(Document_ptr obj,MemBufferedStream &s);
    static Document_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<Document,Document_Helper> Document_var;

#endif
#ifndef __org_w3c_dom_Node__
#define __org_w3c_dom_Node__
  class   Node;
  typedef Node* Node_ptr;
  typedef Node_ptr NodeRef;
  class _proxy_Node;
  class _sk_Node;
  class _nil_Node;

  class Node_Helper {
    public:
    static Node_ptr _nil();
    static CORBA::Boolean is_nil(Node_ptr p);
    static void release(Node_ptr p);
    static void duplicate(Node_ptr p);
    static size_t NP_alignedSize(Node_ptr obj,size_t initialoffset);
    static void marshalObjRef(Node_ptr obj,NetBufferedStream &s);
    static Node_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(Node_ptr obj,MemBufferedStream &s);
    static Node_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<Node,Node_Helper> Node_var;

#endif
#ifndef __org_w3c_dom_NodeList__
#define __org_w3c_dom_NodeList__
  class   NodeList;
  typedef NodeList* NodeList_ptr;
  typedef NodeList_ptr NodeListRef;
  class _proxy_NodeList;
  class _sk_NodeList;
  class _nil_NodeList;

  class NodeList_Helper {
    public:
    static NodeList_ptr _nil();
    static CORBA::Boolean is_nil(NodeList_ptr p);
    static void release(NodeList_ptr p);
    static void duplicate(NodeList_ptr p);
    static size_t NP_alignedSize(NodeList_ptr obj,size_t initialoffset);
    static void marshalObjRef(NodeList_ptr obj,NetBufferedStream &s);
    static NodeList_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(NodeList_ptr obj,MemBufferedStream &s);
    static NodeList_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<NodeList,NodeList_Helper> NodeList_var;

#endif
#ifndef __org_w3c_dom_NamedNodeMap__
#define __org_w3c_dom_NamedNodeMap__
  class   NamedNodeMap;
  typedef NamedNodeMap* NamedNodeMap_ptr;
  typedef NamedNodeMap_ptr NamedNodeMapRef;
  class _proxy_NamedNodeMap;
  class _sk_NamedNodeMap;
  class _nil_NamedNodeMap;

  class NamedNodeMap_Helper {
    public:
    static NamedNodeMap_ptr _nil();
    static CORBA::Boolean is_nil(NamedNodeMap_ptr p);
    static void release(NamedNodeMap_ptr p);
    static void duplicate(NamedNodeMap_ptr p);
    static size_t NP_alignedSize(NamedNodeMap_ptr obj,size_t initialoffset);
    static void marshalObjRef(NamedNodeMap_ptr obj,NetBufferedStream &s);
    static NamedNodeMap_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(NamedNodeMap_ptr obj,MemBufferedStream &s);
    static NamedNodeMap_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<NamedNodeMap,NamedNodeMap_Helper> NamedNodeMap_var;

#endif
#ifndef __org_w3c_dom_CharacterData__
#define __org_w3c_dom_CharacterData__
  class   CharacterData;
  typedef CharacterData* CharacterData_ptr;
  typedef CharacterData_ptr CharacterDataRef;
  class _proxy_CharacterData;
  class _sk_CharacterData;
  class _nil_CharacterData;

  class CharacterData_Helper {
    public:
    static CharacterData_ptr _nil();
    static CORBA::Boolean is_nil(CharacterData_ptr p);
    static void release(CharacterData_ptr p);
    static void duplicate(CharacterData_ptr p);
    static size_t NP_alignedSize(CharacterData_ptr obj,size_t initialoffset);
    static void marshalObjRef(CharacterData_ptr obj,NetBufferedStream &s);
    static CharacterData_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(CharacterData_ptr obj,MemBufferedStream &s);
    static CharacterData_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<CharacterData,CharacterData_Helper> CharacterData_var;

#endif
#ifndef __org_w3c_dom_Attr__
#define __org_w3c_dom_Attr__
  class   Attr;
  typedef Attr* Attr_ptr;
  typedef Attr_ptr AttrRef;
  class _proxy_Attr;
  class _sk_Attr;
  class _nil_Attr;

  class Attr_Helper {
    public:
    static Attr_ptr _nil();
    static CORBA::Boolean is_nil(Attr_ptr p);
    static void release(Attr_ptr p);
    static void duplicate(Attr_ptr p);
    static size_t NP_alignedSize(Attr_ptr obj,size_t initialoffset);
    static void marshalObjRef(Attr_ptr obj,NetBufferedStream &s);
    static Attr_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(Attr_ptr obj,MemBufferedStream &s);
    static Attr_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<Attr,Attr_Helper> Attr_var;

#endif
#ifndef __org_w3c_dom_Element__
#define __org_w3c_dom_Element__
  class   Element;
  typedef Element* Element_ptr;
  typedef Element_ptr ElementRef;
  class _proxy_Element;
  class _sk_Element;
  class _nil_Element;

  class Element_Helper {
    public:
    static Element_ptr _nil();
    static CORBA::Boolean is_nil(Element_ptr p);
    static void release(Element_ptr p);
    static void duplicate(Element_ptr p);
    static size_t NP_alignedSize(Element_ptr obj,size_t initialoffset);
    static void marshalObjRef(Element_ptr obj,NetBufferedStream &s);
    static Element_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(Element_ptr obj,MemBufferedStream &s);
    static Element_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<Element,Element_Helper> Element_var;

#endif
#ifndef __org_w3c_dom_Text__
#define __org_w3c_dom_Text__
  class   Text;
  typedef Text* Text_ptr;
  typedef Text_ptr TextRef;
  class _proxy_Text;
  class _sk_Text;
  class _nil_Text;

  class Text_Helper {
    public:
    static Text_ptr _nil();
    static CORBA::Boolean is_nil(Text_ptr p);
    static void release(Text_ptr p);
    static void duplicate(Text_ptr p);
    static size_t NP_alignedSize(Text_ptr obj,size_t initialoffset);
    static void marshalObjRef(Text_ptr obj,NetBufferedStream &s);
    static Text_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(Text_ptr obj,MemBufferedStream &s);
    static Text_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<Text,Text_Helper> Text_var;

#endif
#ifndef __org_w3c_dom_Comment__
#define __org_w3c_dom_Comment__
  class   Comment;
  typedef Comment* Comment_ptr;
  typedef Comment_ptr CommentRef;
  class _proxy_Comment;
  class _sk_Comment;
  class _nil_Comment;

  class Comment_Helper {
    public:
    static Comment_ptr _nil();
    static CORBA::Boolean is_nil(Comment_ptr p);
    static void release(Comment_ptr p);
    static void duplicate(Comment_ptr p);
    static size_t NP_alignedSize(Comment_ptr obj,size_t initialoffset);
    static void marshalObjRef(Comment_ptr obj,NetBufferedStream &s);
    static Comment_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(Comment_ptr obj,MemBufferedStream &s);
    static Comment_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<Comment,Comment_Helper> Comment_var;

#endif
#ifndef __org_w3c_dom_CDATASection__
#define __org_w3c_dom_CDATASection__
  class   CDATASection;
  typedef CDATASection* CDATASection_ptr;
  typedef CDATASection_ptr CDATASectionRef;
  class _proxy_CDATASection;
  class _sk_CDATASection;
  class _nil_CDATASection;

  class CDATASection_Helper {
    public:
    static CDATASection_ptr _nil();
    static CORBA::Boolean is_nil(CDATASection_ptr p);
    static void release(CDATASection_ptr p);
    static void duplicate(CDATASection_ptr p);
    static size_t NP_alignedSize(CDATASection_ptr obj,size_t initialoffset);
    static void marshalObjRef(CDATASection_ptr obj,NetBufferedStream &s);
    static CDATASection_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(CDATASection_ptr obj,MemBufferedStream &s);
    static CDATASection_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<CDATASection,CDATASection_Helper> CDATASection_var;

#endif
#ifndef __org_w3c_dom_DocumentType__
#define __org_w3c_dom_DocumentType__
  class   DocumentType;
  typedef DocumentType* DocumentType_ptr;
  typedef DocumentType_ptr DocumentTypeRef;
  class _proxy_DocumentType;
  class _sk_DocumentType;
  class _nil_DocumentType;

  class DocumentType_Helper {
    public:
    static DocumentType_ptr _nil();
    static CORBA::Boolean is_nil(DocumentType_ptr p);
    static void release(DocumentType_ptr p);
    static void duplicate(DocumentType_ptr p);
    static size_t NP_alignedSize(DocumentType_ptr obj,size_t initialoffset);
    static void marshalObjRef(DocumentType_ptr obj,NetBufferedStream &s);
    static DocumentType_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(DocumentType_ptr obj,MemBufferedStream &s);
    static DocumentType_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<DocumentType,DocumentType_Helper> DocumentType_var;

#endif
#ifndef __org_w3c_dom_Notation__
#define __org_w3c_dom_Notation__
  class   Notation;
  typedef Notation* Notation_ptr;
  typedef Notation_ptr NotationRef;
  class _proxy_Notation;
  class _sk_Notation;
  class _nil_Notation;

  class Notation_Helper {
    public:
    static Notation_ptr _nil();
    static CORBA::Boolean is_nil(Notation_ptr p);
    static void release(Notation_ptr p);
    static void duplicate(Notation_ptr p);
    static size_t NP_alignedSize(Notation_ptr obj,size_t initialoffset);
    static void marshalObjRef(Notation_ptr obj,NetBufferedStream &s);
    static Notation_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(Notation_ptr obj,MemBufferedStream &s);
    static Notation_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<Notation,Notation_Helper> Notation_var;

#endif
#ifndef __org_w3c_dom_Entity__
#define __org_w3c_dom_Entity__
  class   Entity;
  typedef Entity* Entity_ptr;
  typedef Entity_ptr EntityRef;
  class _proxy_Entity;
  class _sk_Entity;
  class _nil_Entity;

  class Entity_Helper {
    public:
    static Entity_ptr _nil();
    static CORBA::Boolean is_nil(Entity_ptr p);
    static void release(Entity_ptr p);
    static void duplicate(Entity_ptr p);
    static size_t NP_alignedSize(Entity_ptr obj,size_t initialoffset);
    static void marshalObjRef(Entity_ptr obj,NetBufferedStream &s);
    static Entity_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(Entity_ptr obj,MemBufferedStream &s);
    static Entity_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<Entity,Entity_Helper> Entity_var;

#endif
#ifndef __org_w3c_dom_EntityReference__
#define __org_w3c_dom_EntityReference__
  class   EntityReference;
  typedef EntityReference* EntityReference_ptr;
  typedef EntityReference_ptr EntityReferenceRef;
  class _proxy_EntityReference;
  class _sk_EntityReference;
  class _nil_EntityReference;

  class EntityReference_Helper {
    public:
    static EntityReference_ptr _nil();
    static CORBA::Boolean is_nil(EntityReference_ptr p);
    static void release(EntityReference_ptr p);
    static void duplicate(EntityReference_ptr p);
    static size_t NP_alignedSize(EntityReference_ptr obj,size_t initialoffset);
    static void marshalObjRef(EntityReference_ptr obj,NetBufferedStream &s);
    static EntityReference_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(EntityReference_ptr obj,MemBufferedStream &s);
    static EntityReference_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<EntityReference,EntityReference_Helper> EntityReference_var;

#endif
#ifndef __org_w3c_dom_ProcessingInstruction__
#define __org_w3c_dom_ProcessingInstruction__
  class   ProcessingInstruction;
  typedef ProcessingInstruction* ProcessingInstruction_ptr;
  typedef ProcessingInstruction_ptr ProcessingInstructionRef;
  class _proxy_ProcessingInstruction;
  class _sk_ProcessingInstruction;
  class _nil_ProcessingInstruction;

  class ProcessingInstruction_Helper {
    public:
    static ProcessingInstruction_ptr _nil();
    static CORBA::Boolean is_nil(ProcessingInstruction_ptr p);
    static void release(ProcessingInstruction_ptr p);
    static void duplicate(ProcessingInstruction_ptr p);
    static size_t NP_alignedSize(ProcessingInstruction_ptr obj,size_t initialoffset);
    static void marshalObjRef(ProcessingInstruction_ptr obj,NetBufferedStream &s);
    static ProcessingInstruction_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(ProcessingInstruction_ptr obj,MemBufferedStream &s);
    static ProcessingInstruction_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<ProcessingInstruction,ProcessingInstruction_Helper> ProcessingInstruction_var;

#endif
};

#endif // __org-w3c-dom_hh__
