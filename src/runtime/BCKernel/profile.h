#ifndef __profile_h__
#define __profile_h__

#define PROFILE 0

#if PROFILE
# define PROFILE_BEGIN() profile_begin()
# define PROFILE_END()   profile_end()
# define PROFILE_RECORD(a) profile_record(a)

  void profile_begin();
  void profile_end();
  void profile_record(int Tag);

#else
# define PROFILE_BEGIN() 
# define PROFILE_END()
# define PROFILE_RECORD(a)
#endif 

#endif
