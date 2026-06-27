#ifndef __ALC_LEXER_H__
#define __ALC_LEXER_H__

#include <alc/token.h>
#include <alc/defs.h>

typedef struct {
  const char *src;
  usize src_len;
  usize pos, line, llp;
} Alc_Lexer;

ALC_API Alc_Lexer alc_lexer_create(const char *src);

// If tokenization fails, only errors will be put into "out_tokens"
ALC_API b8 alc_lexer_tokenize(Alc_Lexer *lexer, Alc_Token **out_tokens, usize *out_n);

#endif // __ALC_LEXER_H__
