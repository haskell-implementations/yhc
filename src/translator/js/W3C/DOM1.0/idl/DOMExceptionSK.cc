#include "DOMException.hh"


static const char* _0RL_library_version = omniORB_2_5;


const CORBA::UShort DOMExceptionCode::INDEX_SIZE_ERR = 1;
const CORBA::UShort DOMExceptionCode::DOMSTRING_SIZE_ERR = 2;
const CORBA::UShort DOMExceptionCode::HIERARCHY_REQUEST_ERR = 3;
const CORBA::UShort DOMExceptionCode::WRONG_DOCUMENT_ERR = 4;
const CORBA::UShort DOMExceptionCode::INVALID_CHARACTER_ERR = 5;
const CORBA::UShort DOMExceptionCode::NO_DATA_ALLOWED_ERR = 6;
const CORBA::UShort DOMExceptionCode::NO_MODIFICATION_ALLOWED_ERR = 7;
const CORBA::UShort DOMExceptionCode::NOT_FOUND_ERR = 8;
const CORBA::UShort DOMExceptionCode::NOT_SUPPORTED_ERR = 9;
const CORBA::UShort DOMExceptionCode::INUSE_ATTRIBUTE_ERR = 10;
DOMException::DOMException(const DOMException &_s)
{
  code = _s.code;
}

DOMException::DOMException(CORBA::UShort  _code)
{
  code = _code;
}

DOMException & DOMException::operator=(const DOMException &_s)
{
  code = _s.code;
  return *this;
}

size_t
DOMException::NP_alignedSize(size_t _initialoffset)
{
  size_t _msgsize = _initialoffset;
  _msgsize = omni::align_to(_msgsize,omni::ALIGN_2);
  _msgsize += 2;
  return _msgsize;
}

void
DOMException::operator>>= (NetBufferedStream &_n)
{
  code >>= _n;
}

void
DOMException::operator<<= (NetBufferedStream &_n)
{
  code <<= _n;
}

void
DOMException::operator>>= (MemBufferedStream &_n)
{
  code >>= _n;
}

void
DOMException::operator<<= (MemBufferedStream &_n)
{
  code <<= _n;
}

