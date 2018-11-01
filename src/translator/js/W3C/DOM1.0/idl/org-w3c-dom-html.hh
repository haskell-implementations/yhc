#ifndef __org-w3c-dom-html_hh__
#define __org-w3c-dom-html_hh__

#include <omniORB2/CORBA.h>

#include <org-w3c-dom.hh>
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
#include <HTMLCollection.hh>
#include <HTMLDocument.hh>
#include <HTMLElement.hh>
#include <HTMLHtmlElement.hh>
#include <HTMLHeadElement.hh>
#include <HTMLLinkElement.hh>
#include <HTMLTitleElement.hh>
#include <HTMLMetaElement.hh>
#include <HTMLBaseElement.hh>
#include <HTMLIsIndexElement.hh>
#include <HTMLStyleElement.hh>
#include <HTMLBodyElement.hh>
#include <HTMLFormElement.hh>
#include <HTMLSelectElement.hh>
#include <HTMLOptGroupElement.hh>
#include <HTMLOptionElement.hh>
#include <HTMLInputElement.hh>
#include <HTMLTextAreaElement.hh>
#include <HTMLButtonElement.hh>
#include <HTMLLabelElement.hh>
#include <HTMLFieldSetElement.hh>
#include <HTMLLegendElement.hh>
#include <HTMLUListElement.hh>
#include <HTMLOListElement.hh>
#include <HTMLDListElement.hh>
#include <HTMLDirectoryElement.hh>
#include <HTMLMenuElement.hh>
#include <HTMLLIElement.hh>
#include <HTMLBlockquoteElement.hh>
#include <HTMLDivElement.hh>
#include <HTMLParagraphElement.hh>
#include <HTMLHeadingElement.hh>
#include <HTMLQuoteElement.hh>
#include <HTMLPreElement.hh>
#include <HTMLBRElement.hh>
#include <HTMLBaseFontElement.hh>
#include <HTMLFontElement.hh>
#include <HTMLHRElement.hh>
#include <HTMLModElement.hh>
#include <HTMLAnchorElement.hh>
#include <HTMLImageElement.hh>
#include <HTMLObjectElement.hh>
#include <HTMLParamElement.hh>
#include <HTMLAppletElement.hh>
#include <HTMLMapElement.hh>
#include <HTMLAreaElement.hh>
#include <HTMLScriptElement.hh>
#include <HTMLTableElement.hh>
#include <HTMLTableCaptionElement.hh>
#include <HTMLTableColElement.hh>
#include <HTMLTableSectionElement.hh>
#include <HTMLTableRowElement.hh>
#include <HTMLTableCellElement.hh>
#include <HTMLFrameSetElement.hh>
#include <HTMLFrameElement.hh>
#include <HTMLIFrameElement.hh>
_CORBA_MODULE org_w3c_dom_html {
_CORBA_MODULE_PUBLIC

#ifndef __org_w3c_dom_html_HTMLCollection__
#define __org_w3c_dom_html_HTMLCollection__
  class   HTMLCollection;
  typedef HTMLCollection* HTMLCollection_ptr;
  typedef HTMLCollection_ptr HTMLCollectionRef;
  class _proxy_HTMLCollection;
  class _sk_HTMLCollection;
  class _nil_HTMLCollection;

  class HTMLCollection_Helper {
    public:
    static HTMLCollection_ptr _nil();
    static CORBA::Boolean is_nil(HTMLCollection_ptr p);
    static void release(HTMLCollection_ptr p);
    static void duplicate(HTMLCollection_ptr p);
    static size_t NP_alignedSize(HTMLCollection_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLCollection_ptr obj,NetBufferedStream &s);
    static HTMLCollection_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLCollection_ptr obj,MemBufferedStream &s);
    static HTMLCollection_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLCollection,HTMLCollection_Helper> HTMLCollection_var;

#endif
#ifndef __org_w3c_dom_html_HTMLDocument__
#define __org_w3c_dom_html_HTMLDocument__
  class   HTMLDocument;
  typedef HTMLDocument* HTMLDocument_ptr;
  typedef HTMLDocument_ptr HTMLDocumentRef;
  class _proxy_HTMLDocument;
  class _sk_HTMLDocument;
  class _nil_HTMLDocument;

  class HTMLDocument_Helper {
    public:
    static HTMLDocument_ptr _nil();
    static CORBA::Boolean is_nil(HTMLDocument_ptr p);
    static void release(HTMLDocument_ptr p);
    static void duplicate(HTMLDocument_ptr p);
    static size_t NP_alignedSize(HTMLDocument_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLDocument_ptr obj,NetBufferedStream &s);
    static HTMLDocument_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLDocument_ptr obj,MemBufferedStream &s);
    static HTMLDocument_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLDocument,HTMLDocument_Helper> HTMLDocument_var;

#endif
#ifndef __org_w3c_dom_html_HTMLElement__
#define __org_w3c_dom_html_HTMLElement__
  class   HTMLElement;
  typedef HTMLElement* HTMLElement_ptr;
  typedef HTMLElement_ptr HTMLElementRef;
  class _proxy_HTMLElement;
  class _sk_HTMLElement;
  class _nil_HTMLElement;

  class HTMLElement_Helper {
    public:
    static HTMLElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLElement_ptr p);
    static void release(HTMLElement_ptr p);
    static void duplicate(HTMLElement_ptr p);
    static size_t NP_alignedSize(HTMLElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLElement_ptr obj,NetBufferedStream &s);
    static HTMLElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLElement_ptr obj,MemBufferedStream &s);
    static HTMLElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLElement,HTMLElement_Helper> HTMLElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLHtmlElement__
#define __org_w3c_dom_html_HTMLHtmlElement__
  class   HTMLHtmlElement;
  typedef HTMLHtmlElement* HTMLHtmlElement_ptr;
  typedef HTMLHtmlElement_ptr HTMLHtmlElementRef;
  class _proxy_HTMLHtmlElement;
  class _sk_HTMLHtmlElement;
  class _nil_HTMLHtmlElement;

  class HTMLHtmlElement_Helper {
    public:
    static HTMLHtmlElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLHtmlElement_ptr p);
    static void release(HTMLHtmlElement_ptr p);
    static void duplicate(HTMLHtmlElement_ptr p);
    static size_t NP_alignedSize(HTMLHtmlElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLHtmlElement_ptr obj,NetBufferedStream &s);
    static HTMLHtmlElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLHtmlElement_ptr obj,MemBufferedStream &s);
    static HTMLHtmlElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLHtmlElement,HTMLHtmlElement_Helper> HTMLHtmlElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLHeadElement__
#define __org_w3c_dom_html_HTMLHeadElement__
  class   HTMLHeadElement;
  typedef HTMLHeadElement* HTMLHeadElement_ptr;
  typedef HTMLHeadElement_ptr HTMLHeadElementRef;
  class _proxy_HTMLHeadElement;
  class _sk_HTMLHeadElement;
  class _nil_HTMLHeadElement;

  class HTMLHeadElement_Helper {
    public:
    static HTMLHeadElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLHeadElement_ptr p);
    static void release(HTMLHeadElement_ptr p);
    static void duplicate(HTMLHeadElement_ptr p);
    static size_t NP_alignedSize(HTMLHeadElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLHeadElement_ptr obj,NetBufferedStream &s);
    static HTMLHeadElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLHeadElement_ptr obj,MemBufferedStream &s);
    static HTMLHeadElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLHeadElement,HTMLHeadElement_Helper> HTMLHeadElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLLinkElement__
#define __org_w3c_dom_html_HTMLLinkElement__
  class   HTMLLinkElement;
  typedef HTMLLinkElement* HTMLLinkElement_ptr;
  typedef HTMLLinkElement_ptr HTMLLinkElementRef;
  class _proxy_HTMLLinkElement;
  class _sk_HTMLLinkElement;
  class _nil_HTMLLinkElement;

  class HTMLLinkElement_Helper {
    public:
    static HTMLLinkElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLLinkElement_ptr p);
    static void release(HTMLLinkElement_ptr p);
    static void duplicate(HTMLLinkElement_ptr p);
    static size_t NP_alignedSize(HTMLLinkElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLLinkElement_ptr obj,NetBufferedStream &s);
    static HTMLLinkElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLLinkElement_ptr obj,MemBufferedStream &s);
    static HTMLLinkElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLLinkElement,HTMLLinkElement_Helper> HTMLLinkElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLTitleElement__
#define __org_w3c_dom_html_HTMLTitleElement__
  class   HTMLTitleElement;
  typedef HTMLTitleElement* HTMLTitleElement_ptr;
  typedef HTMLTitleElement_ptr HTMLTitleElementRef;
  class _proxy_HTMLTitleElement;
  class _sk_HTMLTitleElement;
  class _nil_HTMLTitleElement;

  class HTMLTitleElement_Helper {
    public:
    static HTMLTitleElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLTitleElement_ptr p);
    static void release(HTMLTitleElement_ptr p);
    static void duplicate(HTMLTitleElement_ptr p);
    static size_t NP_alignedSize(HTMLTitleElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLTitleElement_ptr obj,NetBufferedStream &s);
    static HTMLTitleElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLTitleElement_ptr obj,MemBufferedStream &s);
    static HTMLTitleElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLTitleElement,HTMLTitleElement_Helper> HTMLTitleElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLMetaElement__
#define __org_w3c_dom_html_HTMLMetaElement__
  class   HTMLMetaElement;
  typedef HTMLMetaElement* HTMLMetaElement_ptr;
  typedef HTMLMetaElement_ptr HTMLMetaElementRef;
  class _proxy_HTMLMetaElement;
  class _sk_HTMLMetaElement;
  class _nil_HTMLMetaElement;

  class HTMLMetaElement_Helper {
    public:
    static HTMLMetaElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLMetaElement_ptr p);
    static void release(HTMLMetaElement_ptr p);
    static void duplicate(HTMLMetaElement_ptr p);
    static size_t NP_alignedSize(HTMLMetaElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLMetaElement_ptr obj,NetBufferedStream &s);
    static HTMLMetaElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLMetaElement_ptr obj,MemBufferedStream &s);
    static HTMLMetaElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLMetaElement,HTMLMetaElement_Helper> HTMLMetaElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLBaseElement__
#define __org_w3c_dom_html_HTMLBaseElement__
  class   HTMLBaseElement;
  typedef HTMLBaseElement* HTMLBaseElement_ptr;
  typedef HTMLBaseElement_ptr HTMLBaseElementRef;
  class _proxy_HTMLBaseElement;
  class _sk_HTMLBaseElement;
  class _nil_HTMLBaseElement;

  class HTMLBaseElement_Helper {
    public:
    static HTMLBaseElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLBaseElement_ptr p);
    static void release(HTMLBaseElement_ptr p);
    static void duplicate(HTMLBaseElement_ptr p);
    static size_t NP_alignedSize(HTMLBaseElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLBaseElement_ptr obj,NetBufferedStream &s);
    static HTMLBaseElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLBaseElement_ptr obj,MemBufferedStream &s);
    static HTMLBaseElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLBaseElement,HTMLBaseElement_Helper> HTMLBaseElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLIsIndexElement__
#define __org_w3c_dom_html_HTMLIsIndexElement__
  class   HTMLIsIndexElement;
  typedef HTMLIsIndexElement* HTMLIsIndexElement_ptr;
  typedef HTMLIsIndexElement_ptr HTMLIsIndexElementRef;
  class _proxy_HTMLIsIndexElement;
  class _sk_HTMLIsIndexElement;
  class _nil_HTMLIsIndexElement;

  class HTMLIsIndexElement_Helper {
    public:
    static HTMLIsIndexElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLIsIndexElement_ptr p);
    static void release(HTMLIsIndexElement_ptr p);
    static void duplicate(HTMLIsIndexElement_ptr p);
    static size_t NP_alignedSize(HTMLIsIndexElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLIsIndexElement_ptr obj,NetBufferedStream &s);
    static HTMLIsIndexElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLIsIndexElement_ptr obj,MemBufferedStream &s);
    static HTMLIsIndexElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLIsIndexElement,HTMLIsIndexElement_Helper> HTMLIsIndexElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLStyleElement__
#define __org_w3c_dom_html_HTMLStyleElement__
  class   HTMLStyleElement;
  typedef HTMLStyleElement* HTMLStyleElement_ptr;
  typedef HTMLStyleElement_ptr HTMLStyleElementRef;
  class _proxy_HTMLStyleElement;
  class _sk_HTMLStyleElement;
  class _nil_HTMLStyleElement;

  class HTMLStyleElement_Helper {
    public:
    static HTMLStyleElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLStyleElement_ptr p);
    static void release(HTMLStyleElement_ptr p);
    static void duplicate(HTMLStyleElement_ptr p);
    static size_t NP_alignedSize(HTMLStyleElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLStyleElement_ptr obj,NetBufferedStream &s);
    static HTMLStyleElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLStyleElement_ptr obj,MemBufferedStream &s);
    static HTMLStyleElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLStyleElement,HTMLStyleElement_Helper> HTMLStyleElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLBodyElement__
#define __org_w3c_dom_html_HTMLBodyElement__
  class   HTMLBodyElement;
  typedef HTMLBodyElement* HTMLBodyElement_ptr;
  typedef HTMLBodyElement_ptr HTMLBodyElementRef;
  class _proxy_HTMLBodyElement;
  class _sk_HTMLBodyElement;
  class _nil_HTMLBodyElement;

  class HTMLBodyElement_Helper {
    public:
    static HTMLBodyElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLBodyElement_ptr p);
    static void release(HTMLBodyElement_ptr p);
    static void duplicate(HTMLBodyElement_ptr p);
    static size_t NP_alignedSize(HTMLBodyElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLBodyElement_ptr obj,NetBufferedStream &s);
    static HTMLBodyElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLBodyElement_ptr obj,MemBufferedStream &s);
    static HTMLBodyElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLBodyElement,HTMLBodyElement_Helper> HTMLBodyElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLFormElement__
#define __org_w3c_dom_html_HTMLFormElement__
  class   HTMLFormElement;
  typedef HTMLFormElement* HTMLFormElement_ptr;
  typedef HTMLFormElement_ptr HTMLFormElementRef;
  class _proxy_HTMLFormElement;
  class _sk_HTMLFormElement;
  class _nil_HTMLFormElement;

  class HTMLFormElement_Helper {
    public:
    static HTMLFormElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLFormElement_ptr p);
    static void release(HTMLFormElement_ptr p);
    static void duplicate(HTMLFormElement_ptr p);
    static size_t NP_alignedSize(HTMLFormElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLFormElement_ptr obj,NetBufferedStream &s);
    static HTMLFormElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLFormElement_ptr obj,MemBufferedStream &s);
    static HTMLFormElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLFormElement,HTMLFormElement_Helper> HTMLFormElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLSelectElement__
#define __org_w3c_dom_html_HTMLSelectElement__
  class   HTMLSelectElement;
  typedef HTMLSelectElement* HTMLSelectElement_ptr;
  typedef HTMLSelectElement_ptr HTMLSelectElementRef;
  class _proxy_HTMLSelectElement;
  class _sk_HTMLSelectElement;
  class _nil_HTMLSelectElement;

  class HTMLSelectElement_Helper {
    public:
    static HTMLSelectElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLSelectElement_ptr p);
    static void release(HTMLSelectElement_ptr p);
    static void duplicate(HTMLSelectElement_ptr p);
    static size_t NP_alignedSize(HTMLSelectElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLSelectElement_ptr obj,NetBufferedStream &s);
    static HTMLSelectElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLSelectElement_ptr obj,MemBufferedStream &s);
    static HTMLSelectElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLSelectElement,HTMLSelectElement_Helper> HTMLSelectElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLOptGroupElement__
#define __org_w3c_dom_html_HTMLOptGroupElement__
  class   HTMLOptGroupElement;
  typedef HTMLOptGroupElement* HTMLOptGroupElement_ptr;
  typedef HTMLOptGroupElement_ptr HTMLOptGroupElementRef;
  class _proxy_HTMLOptGroupElement;
  class _sk_HTMLOptGroupElement;
  class _nil_HTMLOptGroupElement;

  class HTMLOptGroupElement_Helper {
    public:
    static HTMLOptGroupElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLOptGroupElement_ptr p);
    static void release(HTMLOptGroupElement_ptr p);
    static void duplicate(HTMLOptGroupElement_ptr p);
    static size_t NP_alignedSize(HTMLOptGroupElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLOptGroupElement_ptr obj,NetBufferedStream &s);
    static HTMLOptGroupElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLOptGroupElement_ptr obj,MemBufferedStream &s);
    static HTMLOptGroupElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLOptGroupElement,HTMLOptGroupElement_Helper> HTMLOptGroupElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLOptionElement__
#define __org_w3c_dom_html_HTMLOptionElement__
  class   HTMLOptionElement;
  typedef HTMLOptionElement* HTMLOptionElement_ptr;
  typedef HTMLOptionElement_ptr HTMLOptionElementRef;
  class _proxy_HTMLOptionElement;
  class _sk_HTMLOptionElement;
  class _nil_HTMLOptionElement;

  class HTMLOptionElement_Helper {
    public:
    static HTMLOptionElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLOptionElement_ptr p);
    static void release(HTMLOptionElement_ptr p);
    static void duplicate(HTMLOptionElement_ptr p);
    static size_t NP_alignedSize(HTMLOptionElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLOptionElement_ptr obj,NetBufferedStream &s);
    static HTMLOptionElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLOptionElement_ptr obj,MemBufferedStream &s);
    static HTMLOptionElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLOptionElement,HTMLOptionElement_Helper> HTMLOptionElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLInputElement__
#define __org_w3c_dom_html_HTMLInputElement__
  class   HTMLInputElement;
  typedef HTMLInputElement* HTMLInputElement_ptr;
  typedef HTMLInputElement_ptr HTMLInputElementRef;
  class _proxy_HTMLInputElement;
  class _sk_HTMLInputElement;
  class _nil_HTMLInputElement;

  class HTMLInputElement_Helper {
    public:
    static HTMLInputElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLInputElement_ptr p);
    static void release(HTMLInputElement_ptr p);
    static void duplicate(HTMLInputElement_ptr p);
    static size_t NP_alignedSize(HTMLInputElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLInputElement_ptr obj,NetBufferedStream &s);
    static HTMLInputElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLInputElement_ptr obj,MemBufferedStream &s);
    static HTMLInputElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLInputElement,HTMLInputElement_Helper> HTMLInputElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLTextAreaElement__
#define __org_w3c_dom_html_HTMLTextAreaElement__
  class   HTMLTextAreaElement;
  typedef HTMLTextAreaElement* HTMLTextAreaElement_ptr;
  typedef HTMLTextAreaElement_ptr HTMLTextAreaElementRef;
  class _proxy_HTMLTextAreaElement;
  class _sk_HTMLTextAreaElement;
  class _nil_HTMLTextAreaElement;

  class HTMLTextAreaElement_Helper {
    public:
    static HTMLTextAreaElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLTextAreaElement_ptr p);
    static void release(HTMLTextAreaElement_ptr p);
    static void duplicate(HTMLTextAreaElement_ptr p);
    static size_t NP_alignedSize(HTMLTextAreaElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLTextAreaElement_ptr obj,NetBufferedStream &s);
    static HTMLTextAreaElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLTextAreaElement_ptr obj,MemBufferedStream &s);
    static HTMLTextAreaElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLTextAreaElement,HTMLTextAreaElement_Helper> HTMLTextAreaElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLButtonElement__
#define __org_w3c_dom_html_HTMLButtonElement__
  class   HTMLButtonElement;
  typedef HTMLButtonElement* HTMLButtonElement_ptr;
  typedef HTMLButtonElement_ptr HTMLButtonElementRef;
  class _proxy_HTMLButtonElement;
  class _sk_HTMLButtonElement;
  class _nil_HTMLButtonElement;

  class HTMLButtonElement_Helper {
    public:
    static HTMLButtonElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLButtonElement_ptr p);
    static void release(HTMLButtonElement_ptr p);
    static void duplicate(HTMLButtonElement_ptr p);
    static size_t NP_alignedSize(HTMLButtonElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLButtonElement_ptr obj,NetBufferedStream &s);
    static HTMLButtonElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLButtonElement_ptr obj,MemBufferedStream &s);
    static HTMLButtonElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLButtonElement,HTMLButtonElement_Helper> HTMLButtonElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLLabelElement__
#define __org_w3c_dom_html_HTMLLabelElement__
  class   HTMLLabelElement;
  typedef HTMLLabelElement* HTMLLabelElement_ptr;
  typedef HTMLLabelElement_ptr HTMLLabelElementRef;
  class _proxy_HTMLLabelElement;
  class _sk_HTMLLabelElement;
  class _nil_HTMLLabelElement;

  class HTMLLabelElement_Helper {
    public:
    static HTMLLabelElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLLabelElement_ptr p);
    static void release(HTMLLabelElement_ptr p);
    static void duplicate(HTMLLabelElement_ptr p);
    static size_t NP_alignedSize(HTMLLabelElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLLabelElement_ptr obj,NetBufferedStream &s);
    static HTMLLabelElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLLabelElement_ptr obj,MemBufferedStream &s);
    static HTMLLabelElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLLabelElement,HTMLLabelElement_Helper> HTMLLabelElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLFieldSetElement__
#define __org_w3c_dom_html_HTMLFieldSetElement__
  class   HTMLFieldSetElement;
  typedef HTMLFieldSetElement* HTMLFieldSetElement_ptr;
  typedef HTMLFieldSetElement_ptr HTMLFieldSetElementRef;
  class _proxy_HTMLFieldSetElement;
  class _sk_HTMLFieldSetElement;
  class _nil_HTMLFieldSetElement;

  class HTMLFieldSetElement_Helper {
    public:
    static HTMLFieldSetElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLFieldSetElement_ptr p);
    static void release(HTMLFieldSetElement_ptr p);
    static void duplicate(HTMLFieldSetElement_ptr p);
    static size_t NP_alignedSize(HTMLFieldSetElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLFieldSetElement_ptr obj,NetBufferedStream &s);
    static HTMLFieldSetElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLFieldSetElement_ptr obj,MemBufferedStream &s);
    static HTMLFieldSetElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLFieldSetElement,HTMLFieldSetElement_Helper> HTMLFieldSetElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLLegendElement__
#define __org_w3c_dom_html_HTMLLegendElement__
  class   HTMLLegendElement;
  typedef HTMLLegendElement* HTMLLegendElement_ptr;
  typedef HTMLLegendElement_ptr HTMLLegendElementRef;
  class _proxy_HTMLLegendElement;
  class _sk_HTMLLegendElement;
  class _nil_HTMLLegendElement;

  class HTMLLegendElement_Helper {
    public:
    static HTMLLegendElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLLegendElement_ptr p);
    static void release(HTMLLegendElement_ptr p);
    static void duplicate(HTMLLegendElement_ptr p);
    static size_t NP_alignedSize(HTMLLegendElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLLegendElement_ptr obj,NetBufferedStream &s);
    static HTMLLegendElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLLegendElement_ptr obj,MemBufferedStream &s);
    static HTMLLegendElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLLegendElement,HTMLLegendElement_Helper> HTMLLegendElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLUListElement__
#define __org_w3c_dom_html_HTMLUListElement__
  class   HTMLUListElement;
  typedef HTMLUListElement* HTMLUListElement_ptr;
  typedef HTMLUListElement_ptr HTMLUListElementRef;
  class _proxy_HTMLUListElement;
  class _sk_HTMLUListElement;
  class _nil_HTMLUListElement;

  class HTMLUListElement_Helper {
    public:
    static HTMLUListElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLUListElement_ptr p);
    static void release(HTMLUListElement_ptr p);
    static void duplicate(HTMLUListElement_ptr p);
    static size_t NP_alignedSize(HTMLUListElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLUListElement_ptr obj,NetBufferedStream &s);
    static HTMLUListElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLUListElement_ptr obj,MemBufferedStream &s);
    static HTMLUListElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLUListElement,HTMLUListElement_Helper> HTMLUListElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLOListElement__
#define __org_w3c_dom_html_HTMLOListElement__
  class   HTMLOListElement;
  typedef HTMLOListElement* HTMLOListElement_ptr;
  typedef HTMLOListElement_ptr HTMLOListElementRef;
  class _proxy_HTMLOListElement;
  class _sk_HTMLOListElement;
  class _nil_HTMLOListElement;

  class HTMLOListElement_Helper {
    public:
    static HTMLOListElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLOListElement_ptr p);
    static void release(HTMLOListElement_ptr p);
    static void duplicate(HTMLOListElement_ptr p);
    static size_t NP_alignedSize(HTMLOListElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLOListElement_ptr obj,NetBufferedStream &s);
    static HTMLOListElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLOListElement_ptr obj,MemBufferedStream &s);
    static HTMLOListElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLOListElement,HTMLOListElement_Helper> HTMLOListElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLDListElement__
#define __org_w3c_dom_html_HTMLDListElement__
  class   HTMLDListElement;
  typedef HTMLDListElement* HTMLDListElement_ptr;
  typedef HTMLDListElement_ptr HTMLDListElementRef;
  class _proxy_HTMLDListElement;
  class _sk_HTMLDListElement;
  class _nil_HTMLDListElement;

  class HTMLDListElement_Helper {
    public:
    static HTMLDListElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLDListElement_ptr p);
    static void release(HTMLDListElement_ptr p);
    static void duplicate(HTMLDListElement_ptr p);
    static size_t NP_alignedSize(HTMLDListElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLDListElement_ptr obj,NetBufferedStream &s);
    static HTMLDListElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLDListElement_ptr obj,MemBufferedStream &s);
    static HTMLDListElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLDListElement,HTMLDListElement_Helper> HTMLDListElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLDirectoryElement__
#define __org_w3c_dom_html_HTMLDirectoryElement__
  class   HTMLDirectoryElement;
  typedef HTMLDirectoryElement* HTMLDirectoryElement_ptr;
  typedef HTMLDirectoryElement_ptr HTMLDirectoryElementRef;
  class _proxy_HTMLDirectoryElement;
  class _sk_HTMLDirectoryElement;
  class _nil_HTMLDirectoryElement;

  class HTMLDirectoryElement_Helper {
    public:
    static HTMLDirectoryElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLDirectoryElement_ptr p);
    static void release(HTMLDirectoryElement_ptr p);
    static void duplicate(HTMLDirectoryElement_ptr p);
    static size_t NP_alignedSize(HTMLDirectoryElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLDirectoryElement_ptr obj,NetBufferedStream &s);
    static HTMLDirectoryElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLDirectoryElement_ptr obj,MemBufferedStream &s);
    static HTMLDirectoryElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLDirectoryElement,HTMLDirectoryElement_Helper> HTMLDirectoryElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLMenuElement__
#define __org_w3c_dom_html_HTMLMenuElement__
  class   HTMLMenuElement;
  typedef HTMLMenuElement* HTMLMenuElement_ptr;
  typedef HTMLMenuElement_ptr HTMLMenuElementRef;
  class _proxy_HTMLMenuElement;
  class _sk_HTMLMenuElement;
  class _nil_HTMLMenuElement;

  class HTMLMenuElement_Helper {
    public:
    static HTMLMenuElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLMenuElement_ptr p);
    static void release(HTMLMenuElement_ptr p);
    static void duplicate(HTMLMenuElement_ptr p);
    static size_t NP_alignedSize(HTMLMenuElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLMenuElement_ptr obj,NetBufferedStream &s);
    static HTMLMenuElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLMenuElement_ptr obj,MemBufferedStream &s);
    static HTMLMenuElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLMenuElement,HTMLMenuElement_Helper> HTMLMenuElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLLIElement__
#define __org_w3c_dom_html_HTMLLIElement__
  class   HTMLLIElement;
  typedef HTMLLIElement* HTMLLIElement_ptr;
  typedef HTMLLIElement_ptr HTMLLIElementRef;
  class _proxy_HTMLLIElement;
  class _sk_HTMLLIElement;
  class _nil_HTMLLIElement;

  class HTMLLIElement_Helper {
    public:
    static HTMLLIElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLLIElement_ptr p);
    static void release(HTMLLIElement_ptr p);
    static void duplicate(HTMLLIElement_ptr p);
    static size_t NP_alignedSize(HTMLLIElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLLIElement_ptr obj,NetBufferedStream &s);
    static HTMLLIElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLLIElement_ptr obj,MemBufferedStream &s);
    static HTMLLIElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLLIElement,HTMLLIElement_Helper> HTMLLIElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLBlockquoteElement__
#define __org_w3c_dom_html_HTMLBlockquoteElement__
  class   HTMLBlockquoteElement;
  typedef HTMLBlockquoteElement* HTMLBlockquoteElement_ptr;
  typedef HTMLBlockquoteElement_ptr HTMLBlockquoteElementRef;
  class _proxy_HTMLBlockquoteElement;
  class _sk_HTMLBlockquoteElement;
  class _nil_HTMLBlockquoteElement;

  class HTMLBlockquoteElement_Helper {
    public:
    static HTMLBlockquoteElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLBlockquoteElement_ptr p);
    static void release(HTMLBlockquoteElement_ptr p);
    static void duplicate(HTMLBlockquoteElement_ptr p);
    static size_t NP_alignedSize(HTMLBlockquoteElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLBlockquoteElement_ptr obj,NetBufferedStream &s);
    static HTMLBlockquoteElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLBlockquoteElement_ptr obj,MemBufferedStream &s);
    static HTMLBlockquoteElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLBlockquoteElement,HTMLBlockquoteElement_Helper> HTMLBlockquoteElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLDivElement__
#define __org_w3c_dom_html_HTMLDivElement__
  class   HTMLDivElement;
  typedef HTMLDivElement* HTMLDivElement_ptr;
  typedef HTMLDivElement_ptr HTMLDivElementRef;
  class _proxy_HTMLDivElement;
  class _sk_HTMLDivElement;
  class _nil_HTMLDivElement;

  class HTMLDivElement_Helper {
    public:
    static HTMLDivElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLDivElement_ptr p);
    static void release(HTMLDivElement_ptr p);
    static void duplicate(HTMLDivElement_ptr p);
    static size_t NP_alignedSize(HTMLDivElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLDivElement_ptr obj,NetBufferedStream &s);
    static HTMLDivElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLDivElement_ptr obj,MemBufferedStream &s);
    static HTMLDivElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLDivElement,HTMLDivElement_Helper> HTMLDivElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLParagraphElement__
#define __org_w3c_dom_html_HTMLParagraphElement__
  class   HTMLParagraphElement;
  typedef HTMLParagraphElement* HTMLParagraphElement_ptr;
  typedef HTMLParagraphElement_ptr HTMLParagraphElementRef;
  class _proxy_HTMLParagraphElement;
  class _sk_HTMLParagraphElement;
  class _nil_HTMLParagraphElement;

  class HTMLParagraphElement_Helper {
    public:
    static HTMLParagraphElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLParagraphElement_ptr p);
    static void release(HTMLParagraphElement_ptr p);
    static void duplicate(HTMLParagraphElement_ptr p);
    static size_t NP_alignedSize(HTMLParagraphElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLParagraphElement_ptr obj,NetBufferedStream &s);
    static HTMLParagraphElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLParagraphElement_ptr obj,MemBufferedStream &s);
    static HTMLParagraphElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLParagraphElement,HTMLParagraphElement_Helper> HTMLParagraphElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLHeadingElement__
#define __org_w3c_dom_html_HTMLHeadingElement__
  class   HTMLHeadingElement;
  typedef HTMLHeadingElement* HTMLHeadingElement_ptr;
  typedef HTMLHeadingElement_ptr HTMLHeadingElementRef;
  class _proxy_HTMLHeadingElement;
  class _sk_HTMLHeadingElement;
  class _nil_HTMLHeadingElement;

  class HTMLHeadingElement_Helper {
    public:
    static HTMLHeadingElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLHeadingElement_ptr p);
    static void release(HTMLHeadingElement_ptr p);
    static void duplicate(HTMLHeadingElement_ptr p);
    static size_t NP_alignedSize(HTMLHeadingElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLHeadingElement_ptr obj,NetBufferedStream &s);
    static HTMLHeadingElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLHeadingElement_ptr obj,MemBufferedStream &s);
    static HTMLHeadingElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLHeadingElement,HTMLHeadingElement_Helper> HTMLHeadingElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLQuoteElement__
#define __org_w3c_dom_html_HTMLQuoteElement__
  class   HTMLQuoteElement;
  typedef HTMLQuoteElement* HTMLQuoteElement_ptr;
  typedef HTMLQuoteElement_ptr HTMLQuoteElementRef;
  class _proxy_HTMLQuoteElement;
  class _sk_HTMLQuoteElement;
  class _nil_HTMLQuoteElement;

  class HTMLQuoteElement_Helper {
    public:
    static HTMLQuoteElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLQuoteElement_ptr p);
    static void release(HTMLQuoteElement_ptr p);
    static void duplicate(HTMLQuoteElement_ptr p);
    static size_t NP_alignedSize(HTMLQuoteElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLQuoteElement_ptr obj,NetBufferedStream &s);
    static HTMLQuoteElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLQuoteElement_ptr obj,MemBufferedStream &s);
    static HTMLQuoteElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLQuoteElement,HTMLQuoteElement_Helper> HTMLQuoteElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLPreElement__
#define __org_w3c_dom_html_HTMLPreElement__
  class   HTMLPreElement;
  typedef HTMLPreElement* HTMLPreElement_ptr;
  typedef HTMLPreElement_ptr HTMLPreElementRef;
  class _proxy_HTMLPreElement;
  class _sk_HTMLPreElement;
  class _nil_HTMLPreElement;

  class HTMLPreElement_Helper {
    public:
    static HTMLPreElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLPreElement_ptr p);
    static void release(HTMLPreElement_ptr p);
    static void duplicate(HTMLPreElement_ptr p);
    static size_t NP_alignedSize(HTMLPreElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLPreElement_ptr obj,NetBufferedStream &s);
    static HTMLPreElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLPreElement_ptr obj,MemBufferedStream &s);
    static HTMLPreElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLPreElement,HTMLPreElement_Helper> HTMLPreElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLBRElement__
#define __org_w3c_dom_html_HTMLBRElement__
  class   HTMLBRElement;
  typedef HTMLBRElement* HTMLBRElement_ptr;
  typedef HTMLBRElement_ptr HTMLBRElementRef;
  class _proxy_HTMLBRElement;
  class _sk_HTMLBRElement;
  class _nil_HTMLBRElement;

  class HTMLBRElement_Helper {
    public:
    static HTMLBRElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLBRElement_ptr p);
    static void release(HTMLBRElement_ptr p);
    static void duplicate(HTMLBRElement_ptr p);
    static size_t NP_alignedSize(HTMLBRElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLBRElement_ptr obj,NetBufferedStream &s);
    static HTMLBRElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLBRElement_ptr obj,MemBufferedStream &s);
    static HTMLBRElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLBRElement,HTMLBRElement_Helper> HTMLBRElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLBaseFontElement__
#define __org_w3c_dom_html_HTMLBaseFontElement__
  class   HTMLBaseFontElement;
  typedef HTMLBaseFontElement* HTMLBaseFontElement_ptr;
  typedef HTMLBaseFontElement_ptr HTMLBaseFontElementRef;
  class _proxy_HTMLBaseFontElement;
  class _sk_HTMLBaseFontElement;
  class _nil_HTMLBaseFontElement;

  class HTMLBaseFontElement_Helper {
    public:
    static HTMLBaseFontElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLBaseFontElement_ptr p);
    static void release(HTMLBaseFontElement_ptr p);
    static void duplicate(HTMLBaseFontElement_ptr p);
    static size_t NP_alignedSize(HTMLBaseFontElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLBaseFontElement_ptr obj,NetBufferedStream &s);
    static HTMLBaseFontElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLBaseFontElement_ptr obj,MemBufferedStream &s);
    static HTMLBaseFontElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLBaseFontElement,HTMLBaseFontElement_Helper> HTMLBaseFontElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLFontElement__
#define __org_w3c_dom_html_HTMLFontElement__
  class   HTMLFontElement;
  typedef HTMLFontElement* HTMLFontElement_ptr;
  typedef HTMLFontElement_ptr HTMLFontElementRef;
  class _proxy_HTMLFontElement;
  class _sk_HTMLFontElement;
  class _nil_HTMLFontElement;

  class HTMLFontElement_Helper {
    public:
    static HTMLFontElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLFontElement_ptr p);
    static void release(HTMLFontElement_ptr p);
    static void duplicate(HTMLFontElement_ptr p);
    static size_t NP_alignedSize(HTMLFontElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLFontElement_ptr obj,NetBufferedStream &s);
    static HTMLFontElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLFontElement_ptr obj,MemBufferedStream &s);
    static HTMLFontElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLFontElement,HTMLFontElement_Helper> HTMLFontElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLHRElement__
#define __org_w3c_dom_html_HTMLHRElement__
  class   HTMLHRElement;
  typedef HTMLHRElement* HTMLHRElement_ptr;
  typedef HTMLHRElement_ptr HTMLHRElementRef;
  class _proxy_HTMLHRElement;
  class _sk_HTMLHRElement;
  class _nil_HTMLHRElement;

  class HTMLHRElement_Helper {
    public:
    static HTMLHRElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLHRElement_ptr p);
    static void release(HTMLHRElement_ptr p);
    static void duplicate(HTMLHRElement_ptr p);
    static size_t NP_alignedSize(HTMLHRElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLHRElement_ptr obj,NetBufferedStream &s);
    static HTMLHRElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLHRElement_ptr obj,MemBufferedStream &s);
    static HTMLHRElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLHRElement,HTMLHRElement_Helper> HTMLHRElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLModElement__
#define __org_w3c_dom_html_HTMLModElement__
  class   HTMLModElement;
  typedef HTMLModElement* HTMLModElement_ptr;
  typedef HTMLModElement_ptr HTMLModElementRef;
  class _proxy_HTMLModElement;
  class _sk_HTMLModElement;
  class _nil_HTMLModElement;

  class HTMLModElement_Helper {
    public:
    static HTMLModElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLModElement_ptr p);
    static void release(HTMLModElement_ptr p);
    static void duplicate(HTMLModElement_ptr p);
    static size_t NP_alignedSize(HTMLModElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLModElement_ptr obj,NetBufferedStream &s);
    static HTMLModElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLModElement_ptr obj,MemBufferedStream &s);
    static HTMLModElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLModElement,HTMLModElement_Helper> HTMLModElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLAnchorElement__
#define __org_w3c_dom_html_HTMLAnchorElement__
  class   HTMLAnchorElement;
  typedef HTMLAnchorElement* HTMLAnchorElement_ptr;
  typedef HTMLAnchorElement_ptr HTMLAnchorElementRef;
  class _proxy_HTMLAnchorElement;
  class _sk_HTMLAnchorElement;
  class _nil_HTMLAnchorElement;

  class HTMLAnchorElement_Helper {
    public:
    static HTMLAnchorElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLAnchorElement_ptr p);
    static void release(HTMLAnchorElement_ptr p);
    static void duplicate(HTMLAnchorElement_ptr p);
    static size_t NP_alignedSize(HTMLAnchorElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLAnchorElement_ptr obj,NetBufferedStream &s);
    static HTMLAnchorElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLAnchorElement_ptr obj,MemBufferedStream &s);
    static HTMLAnchorElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLAnchorElement,HTMLAnchorElement_Helper> HTMLAnchorElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLImageElement__
#define __org_w3c_dom_html_HTMLImageElement__
  class   HTMLImageElement;
  typedef HTMLImageElement* HTMLImageElement_ptr;
  typedef HTMLImageElement_ptr HTMLImageElementRef;
  class _proxy_HTMLImageElement;
  class _sk_HTMLImageElement;
  class _nil_HTMLImageElement;

  class HTMLImageElement_Helper {
    public:
    static HTMLImageElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLImageElement_ptr p);
    static void release(HTMLImageElement_ptr p);
    static void duplicate(HTMLImageElement_ptr p);
    static size_t NP_alignedSize(HTMLImageElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLImageElement_ptr obj,NetBufferedStream &s);
    static HTMLImageElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLImageElement_ptr obj,MemBufferedStream &s);
    static HTMLImageElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLImageElement,HTMLImageElement_Helper> HTMLImageElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLObjectElement__
#define __org_w3c_dom_html_HTMLObjectElement__
  class   HTMLObjectElement;
  typedef HTMLObjectElement* HTMLObjectElement_ptr;
  typedef HTMLObjectElement_ptr HTMLObjectElementRef;
  class _proxy_HTMLObjectElement;
  class _sk_HTMLObjectElement;
  class _nil_HTMLObjectElement;

  class HTMLObjectElement_Helper {
    public:
    static HTMLObjectElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLObjectElement_ptr p);
    static void release(HTMLObjectElement_ptr p);
    static void duplicate(HTMLObjectElement_ptr p);
    static size_t NP_alignedSize(HTMLObjectElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLObjectElement_ptr obj,NetBufferedStream &s);
    static HTMLObjectElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLObjectElement_ptr obj,MemBufferedStream &s);
    static HTMLObjectElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLObjectElement,HTMLObjectElement_Helper> HTMLObjectElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLParamElement__
#define __org_w3c_dom_html_HTMLParamElement__
  class   HTMLParamElement;
  typedef HTMLParamElement* HTMLParamElement_ptr;
  typedef HTMLParamElement_ptr HTMLParamElementRef;
  class _proxy_HTMLParamElement;
  class _sk_HTMLParamElement;
  class _nil_HTMLParamElement;

  class HTMLParamElement_Helper {
    public:
    static HTMLParamElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLParamElement_ptr p);
    static void release(HTMLParamElement_ptr p);
    static void duplicate(HTMLParamElement_ptr p);
    static size_t NP_alignedSize(HTMLParamElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLParamElement_ptr obj,NetBufferedStream &s);
    static HTMLParamElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLParamElement_ptr obj,MemBufferedStream &s);
    static HTMLParamElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLParamElement,HTMLParamElement_Helper> HTMLParamElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLAppletElement__
#define __org_w3c_dom_html_HTMLAppletElement__
  class   HTMLAppletElement;
  typedef HTMLAppletElement* HTMLAppletElement_ptr;
  typedef HTMLAppletElement_ptr HTMLAppletElementRef;
  class _proxy_HTMLAppletElement;
  class _sk_HTMLAppletElement;
  class _nil_HTMLAppletElement;

  class HTMLAppletElement_Helper {
    public:
    static HTMLAppletElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLAppletElement_ptr p);
    static void release(HTMLAppletElement_ptr p);
    static void duplicate(HTMLAppletElement_ptr p);
    static size_t NP_alignedSize(HTMLAppletElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLAppletElement_ptr obj,NetBufferedStream &s);
    static HTMLAppletElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLAppletElement_ptr obj,MemBufferedStream &s);
    static HTMLAppletElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLAppletElement,HTMLAppletElement_Helper> HTMLAppletElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLMapElement__
#define __org_w3c_dom_html_HTMLMapElement__
  class   HTMLMapElement;
  typedef HTMLMapElement* HTMLMapElement_ptr;
  typedef HTMLMapElement_ptr HTMLMapElementRef;
  class _proxy_HTMLMapElement;
  class _sk_HTMLMapElement;
  class _nil_HTMLMapElement;

  class HTMLMapElement_Helper {
    public:
    static HTMLMapElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLMapElement_ptr p);
    static void release(HTMLMapElement_ptr p);
    static void duplicate(HTMLMapElement_ptr p);
    static size_t NP_alignedSize(HTMLMapElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLMapElement_ptr obj,NetBufferedStream &s);
    static HTMLMapElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLMapElement_ptr obj,MemBufferedStream &s);
    static HTMLMapElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLMapElement,HTMLMapElement_Helper> HTMLMapElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLAreaElement__
#define __org_w3c_dom_html_HTMLAreaElement__
  class   HTMLAreaElement;
  typedef HTMLAreaElement* HTMLAreaElement_ptr;
  typedef HTMLAreaElement_ptr HTMLAreaElementRef;
  class _proxy_HTMLAreaElement;
  class _sk_HTMLAreaElement;
  class _nil_HTMLAreaElement;

  class HTMLAreaElement_Helper {
    public:
    static HTMLAreaElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLAreaElement_ptr p);
    static void release(HTMLAreaElement_ptr p);
    static void duplicate(HTMLAreaElement_ptr p);
    static size_t NP_alignedSize(HTMLAreaElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLAreaElement_ptr obj,NetBufferedStream &s);
    static HTMLAreaElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLAreaElement_ptr obj,MemBufferedStream &s);
    static HTMLAreaElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLAreaElement,HTMLAreaElement_Helper> HTMLAreaElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLScriptElement__
#define __org_w3c_dom_html_HTMLScriptElement__
  class   HTMLScriptElement;
  typedef HTMLScriptElement* HTMLScriptElement_ptr;
  typedef HTMLScriptElement_ptr HTMLScriptElementRef;
  class _proxy_HTMLScriptElement;
  class _sk_HTMLScriptElement;
  class _nil_HTMLScriptElement;

  class HTMLScriptElement_Helper {
    public:
    static HTMLScriptElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLScriptElement_ptr p);
    static void release(HTMLScriptElement_ptr p);
    static void duplicate(HTMLScriptElement_ptr p);
    static size_t NP_alignedSize(HTMLScriptElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLScriptElement_ptr obj,NetBufferedStream &s);
    static HTMLScriptElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLScriptElement_ptr obj,MemBufferedStream &s);
    static HTMLScriptElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLScriptElement,HTMLScriptElement_Helper> HTMLScriptElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLTableElement__
#define __org_w3c_dom_html_HTMLTableElement__
  class   HTMLTableElement;
  typedef HTMLTableElement* HTMLTableElement_ptr;
  typedef HTMLTableElement_ptr HTMLTableElementRef;
  class _proxy_HTMLTableElement;
  class _sk_HTMLTableElement;
  class _nil_HTMLTableElement;

  class HTMLTableElement_Helper {
    public:
    static HTMLTableElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLTableElement_ptr p);
    static void release(HTMLTableElement_ptr p);
    static void duplicate(HTMLTableElement_ptr p);
    static size_t NP_alignedSize(HTMLTableElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLTableElement_ptr obj,NetBufferedStream &s);
    static HTMLTableElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLTableElement_ptr obj,MemBufferedStream &s);
    static HTMLTableElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLTableElement,HTMLTableElement_Helper> HTMLTableElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLTableCaptionElement__
#define __org_w3c_dom_html_HTMLTableCaptionElement__
  class   HTMLTableCaptionElement;
  typedef HTMLTableCaptionElement* HTMLTableCaptionElement_ptr;
  typedef HTMLTableCaptionElement_ptr HTMLTableCaptionElementRef;
  class _proxy_HTMLTableCaptionElement;
  class _sk_HTMLTableCaptionElement;
  class _nil_HTMLTableCaptionElement;

  class HTMLTableCaptionElement_Helper {
    public:
    static HTMLTableCaptionElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLTableCaptionElement_ptr p);
    static void release(HTMLTableCaptionElement_ptr p);
    static void duplicate(HTMLTableCaptionElement_ptr p);
    static size_t NP_alignedSize(HTMLTableCaptionElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLTableCaptionElement_ptr obj,NetBufferedStream &s);
    static HTMLTableCaptionElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLTableCaptionElement_ptr obj,MemBufferedStream &s);
    static HTMLTableCaptionElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLTableCaptionElement,HTMLTableCaptionElement_Helper> HTMLTableCaptionElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLTableColElement__
#define __org_w3c_dom_html_HTMLTableColElement__
  class   HTMLTableColElement;
  typedef HTMLTableColElement* HTMLTableColElement_ptr;
  typedef HTMLTableColElement_ptr HTMLTableColElementRef;
  class _proxy_HTMLTableColElement;
  class _sk_HTMLTableColElement;
  class _nil_HTMLTableColElement;

  class HTMLTableColElement_Helper {
    public:
    static HTMLTableColElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLTableColElement_ptr p);
    static void release(HTMLTableColElement_ptr p);
    static void duplicate(HTMLTableColElement_ptr p);
    static size_t NP_alignedSize(HTMLTableColElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLTableColElement_ptr obj,NetBufferedStream &s);
    static HTMLTableColElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLTableColElement_ptr obj,MemBufferedStream &s);
    static HTMLTableColElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLTableColElement,HTMLTableColElement_Helper> HTMLTableColElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLTableSectionElement__
#define __org_w3c_dom_html_HTMLTableSectionElement__
  class   HTMLTableSectionElement;
  typedef HTMLTableSectionElement* HTMLTableSectionElement_ptr;
  typedef HTMLTableSectionElement_ptr HTMLTableSectionElementRef;
  class _proxy_HTMLTableSectionElement;
  class _sk_HTMLTableSectionElement;
  class _nil_HTMLTableSectionElement;

  class HTMLTableSectionElement_Helper {
    public:
    static HTMLTableSectionElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLTableSectionElement_ptr p);
    static void release(HTMLTableSectionElement_ptr p);
    static void duplicate(HTMLTableSectionElement_ptr p);
    static size_t NP_alignedSize(HTMLTableSectionElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLTableSectionElement_ptr obj,NetBufferedStream &s);
    static HTMLTableSectionElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLTableSectionElement_ptr obj,MemBufferedStream &s);
    static HTMLTableSectionElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLTableSectionElement,HTMLTableSectionElement_Helper> HTMLTableSectionElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLTableRowElement__
#define __org_w3c_dom_html_HTMLTableRowElement__
  class   HTMLTableRowElement;
  typedef HTMLTableRowElement* HTMLTableRowElement_ptr;
  typedef HTMLTableRowElement_ptr HTMLTableRowElementRef;
  class _proxy_HTMLTableRowElement;
  class _sk_HTMLTableRowElement;
  class _nil_HTMLTableRowElement;

  class HTMLTableRowElement_Helper {
    public:
    static HTMLTableRowElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLTableRowElement_ptr p);
    static void release(HTMLTableRowElement_ptr p);
    static void duplicate(HTMLTableRowElement_ptr p);
    static size_t NP_alignedSize(HTMLTableRowElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLTableRowElement_ptr obj,NetBufferedStream &s);
    static HTMLTableRowElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLTableRowElement_ptr obj,MemBufferedStream &s);
    static HTMLTableRowElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLTableRowElement,HTMLTableRowElement_Helper> HTMLTableRowElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLTableCellElement__
#define __org_w3c_dom_html_HTMLTableCellElement__
  class   HTMLTableCellElement;
  typedef HTMLTableCellElement* HTMLTableCellElement_ptr;
  typedef HTMLTableCellElement_ptr HTMLTableCellElementRef;
  class _proxy_HTMLTableCellElement;
  class _sk_HTMLTableCellElement;
  class _nil_HTMLTableCellElement;

  class HTMLTableCellElement_Helper {
    public:
    static HTMLTableCellElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLTableCellElement_ptr p);
    static void release(HTMLTableCellElement_ptr p);
    static void duplicate(HTMLTableCellElement_ptr p);
    static size_t NP_alignedSize(HTMLTableCellElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLTableCellElement_ptr obj,NetBufferedStream &s);
    static HTMLTableCellElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLTableCellElement_ptr obj,MemBufferedStream &s);
    static HTMLTableCellElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLTableCellElement,HTMLTableCellElement_Helper> HTMLTableCellElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLFrameSetElement__
#define __org_w3c_dom_html_HTMLFrameSetElement__
  class   HTMLFrameSetElement;
  typedef HTMLFrameSetElement* HTMLFrameSetElement_ptr;
  typedef HTMLFrameSetElement_ptr HTMLFrameSetElementRef;
  class _proxy_HTMLFrameSetElement;
  class _sk_HTMLFrameSetElement;
  class _nil_HTMLFrameSetElement;

  class HTMLFrameSetElement_Helper {
    public:
    static HTMLFrameSetElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLFrameSetElement_ptr p);
    static void release(HTMLFrameSetElement_ptr p);
    static void duplicate(HTMLFrameSetElement_ptr p);
    static size_t NP_alignedSize(HTMLFrameSetElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLFrameSetElement_ptr obj,NetBufferedStream &s);
    static HTMLFrameSetElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLFrameSetElement_ptr obj,MemBufferedStream &s);
    static HTMLFrameSetElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLFrameSetElement,HTMLFrameSetElement_Helper> HTMLFrameSetElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLFrameElement__
#define __org_w3c_dom_html_HTMLFrameElement__
  class   HTMLFrameElement;
  typedef HTMLFrameElement* HTMLFrameElement_ptr;
  typedef HTMLFrameElement_ptr HTMLFrameElementRef;
  class _proxy_HTMLFrameElement;
  class _sk_HTMLFrameElement;
  class _nil_HTMLFrameElement;

  class HTMLFrameElement_Helper {
    public:
    static HTMLFrameElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLFrameElement_ptr p);
    static void release(HTMLFrameElement_ptr p);
    static void duplicate(HTMLFrameElement_ptr p);
    static size_t NP_alignedSize(HTMLFrameElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLFrameElement_ptr obj,NetBufferedStream &s);
    static HTMLFrameElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLFrameElement_ptr obj,MemBufferedStream &s);
    static HTMLFrameElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLFrameElement,HTMLFrameElement_Helper> HTMLFrameElement_var;

#endif
#ifndef __org_w3c_dom_html_HTMLIFrameElement__
#define __org_w3c_dom_html_HTMLIFrameElement__
  class   HTMLIFrameElement;
  typedef HTMLIFrameElement* HTMLIFrameElement_ptr;
  typedef HTMLIFrameElement_ptr HTMLIFrameElementRef;
  class _proxy_HTMLIFrameElement;
  class _sk_HTMLIFrameElement;
  class _nil_HTMLIFrameElement;

  class HTMLIFrameElement_Helper {
    public:
    static HTMLIFrameElement_ptr _nil();
    static CORBA::Boolean is_nil(HTMLIFrameElement_ptr p);
    static void release(HTMLIFrameElement_ptr p);
    static void duplicate(HTMLIFrameElement_ptr p);
    static size_t NP_alignedSize(HTMLIFrameElement_ptr obj,size_t initialoffset);
    static void marshalObjRef(HTMLIFrameElement_ptr obj,NetBufferedStream &s);
    static HTMLIFrameElement_ptr unmarshalObjRef(NetBufferedStream &s);
    static void marshalObjRef(HTMLIFrameElement_ptr obj,MemBufferedStream &s);
    static HTMLIFrameElement_ptr unmarshalObjRef(MemBufferedStream &s);
  };
  typedef _CORBA_ObjRef_Var<HTMLIFrameElement,HTMLIFrameElement_Helper> HTMLIFrameElement_var;

#endif
  typedef _CORBA_Unbounded_Sequence_w_FixSizeElement<CORBA::UShort,2,2> DOMString;
  typedef _CORBA_Sequence_Var<DOMString, CORBA::UShort > DOMString_var;

  typedef org_w3c_dom::Node Node;
  typedef org_w3c_dom::Node_ptr Node_ptr;
  typedef org_w3c_dom::NodeRef NodeRef;
  typedef org_w3c_dom::Node_Helper Node_Helper;
  typedef org_w3c_dom::_proxy_Node _proxy_Node;
  typedef org_w3c_dom::_sk_Node _sk_Node;
  typedef org_w3c_dom::_nil_Node _nil_Node;
#define org_w3c_dom_html_Node_IntfRepoID org_w3c_dom_Node_IntfRepoID;
  typedef org_w3c_dom::Node_var Node_var;
  typedef org_w3c_dom::NodeList NodeList;
  typedef org_w3c_dom::NodeList_ptr NodeList_ptr;
  typedef org_w3c_dom::NodeListRef NodeListRef;
  typedef org_w3c_dom::NodeList_Helper NodeList_Helper;
  typedef org_w3c_dom::_proxy_NodeList _proxy_NodeList;
  typedef org_w3c_dom::_sk_NodeList _sk_NodeList;
  typedef org_w3c_dom::_nil_NodeList _nil_NodeList;
#define org_w3c_dom_html_NodeList_IntfRepoID org_w3c_dom_NodeList_IntfRepoID;
  typedef org_w3c_dom::NodeList_var NodeList_var;
  typedef org_w3c_dom::Document Document;
  typedef org_w3c_dom::Document_ptr Document_ptr;
  typedef org_w3c_dom::DocumentRef DocumentRef;
  typedef org_w3c_dom::Document_Helper Document_Helper;
  typedef org_w3c_dom::_proxy_Document _proxy_Document;
  typedef org_w3c_dom::_sk_Document _sk_Document;
  typedef org_w3c_dom::_nil_Document _nil_Document;
#define org_w3c_dom_html_Document_IntfRepoID org_w3c_dom_Document_IntfRepoID;
  typedef org_w3c_dom::Document_var Document_var;
  typedef org_w3c_dom::Element Element;
  typedef org_w3c_dom::Element_ptr Element_ptr;
  typedef org_w3c_dom::ElementRef ElementRef;
  typedef org_w3c_dom::Element_Helper Element_Helper;
  typedef org_w3c_dom::_proxy_Element _proxy_Element;
  typedef org_w3c_dom::_sk_Element _sk_Element;
  typedef org_w3c_dom::_nil_Element _nil_Element;
#define org_w3c_dom_html_Element_IntfRepoID org_w3c_dom_Element_IntfRepoID;
  typedef org_w3c_dom::Element_var Element_var;
};

#endif // __org-w3c-dom-html_hh__
