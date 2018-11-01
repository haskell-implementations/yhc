#ifndef __DOMException_hh__
#define __DOMException_hh__

#include <omniORB2/CORBA.h>

_CORBA_MODULE DOMExceptionCode {
_CORBA_MODULE_PUBLIC

  static const CORBA::UShort INDEX_SIZE_ERR;
  static const CORBA::UShort DOMSTRING_SIZE_ERR;
  static const CORBA::UShort HIERARCHY_REQUEST_ERR;
  static const CORBA::UShort WRONG_DOCUMENT_ERR;
  static const CORBA::UShort INVALID_CHARACTER_ERR;
  static const CORBA::UShort NO_DATA_ALLOWED_ERR;
  static const CORBA::UShort NO_MODIFICATION_ALLOWED_ERR;
  static const CORBA::UShort NOT_FOUND_ERR;
  static const CORBA::UShort NOT_SUPPORTED_ERR;
  static const CORBA::UShort INUSE_ATTRIBUTE_ERR;
};

#define DOMException_IntfRepoID "IDL:DOMException:1.0"

class DOMException : public CORBA::UserException {
public:

  CORBA::UShort code;
  
  DOMException() {};
  DOMException(const DOMException &);
  DOMException(CORBA::UShort  _code);
  DOMException & operator=(const DOMException &);
  virtual ~DOMException() {};
  size_t NP_alignedSize(size_t initialoffset);
  void operator>>= (NetBufferedStream &);
  void operator<<= (NetBufferedStream &);
  void operator>>= (MemBufferedStream &);
  void operator<<= (MemBufferedStream &);
};

#endif // __DOMException_hh__
