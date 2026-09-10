#ifndef __ALC_PROGRAM_PRIVATE_H__
#define __ALC_PROGRAM_PRIVATE_H__

#include "alc/program.h"
#include "alc/vector.h"

static inline void alc_program_add_entry(Alc_Vector(Alc_Entry) entries, Alc_Entry entry)
{
  alc_vector_push(entries, entry);
}

#endif // __ALC_PROGRAM_PRIVATE_H__
