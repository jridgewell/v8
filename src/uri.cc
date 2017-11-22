// Copyright 2016 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.

#include "src/uri.h"

#include <vector>

#include "src/allocation.h"
#include "src/char-predicates-inl.h"
#include "src/handles.h"
#include "src/isolate-inl.h"
#include "src/string-search.h"
#include "src/unicode-inl.h"

namespace v8 {
namespace internal {

namespace {  // anonymous namespace for DecodeURI helper functions
bool IsReservedPredicate(const uc16 c) {
  switch (c) {
    case '#':
    case '$':
    case '&':
    case '+':
    case ',':
    case '/':
    case ':':
    case ';':
    case '=':
    case '?':
    case '@':
      return true;
    default:
      return false;
  }
}


/**
 * Decodes a single hex char into it's equivalent decimal value.
 * The value is then left shifted by `shift`.
 * If an invalid hex char is encountered, this returns `255`, which is guaranteed
 * to be rejected by the decoder FSM later on.
 */
uint8_t HexCharToInt(const uc16 c, const uint8_t shift = 0) {
  switch (c) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      return (c - '0') << shift;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
      return (c - 'A' + 10) << shift;
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
      return (c - 'a' + 10) << shift;
    default:
      return 255;
  }
}

/**
 * The below algorithm is based on Bjoern Hoehrmann's DFA Unicode Decoder.
 * Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
 * See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
 */
constexpr uint8_t kUtf8Accept = 12;
constexpr uint8_t kUtf8Reject = 0;
uc32 Utf8Decode(const uc32 codep, const uint8_t byte, uint8_t *const state) {
  static constexpr uint8_t kUtf8Data[] = {
      // The first part of the table maps character byte to a transition.
       0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0, 0,
       1, 1, 1, 1,  1, 1, 1, 1,   1, 1, 1, 1, 1, 1, 1, 1,
       2, 2, 2, 2,  2, 2, 2, 2,   2, 2, 2, 2, 2, 2, 2, 2,
       3, 3, 3, 3,  3, 3, 3, 3,   3, 3, 3, 3, 3, 3, 3, 3,
       3, 3, 3, 3,  3, 3, 3, 3,   3, 3, 3, 3, 3, 3, 3, 3,
       4, 4, 5, 5,  5, 5, 5, 5,   5, 5, 5, 5, 5, 5, 5, 5,
       5, 5, 5, 5,  5, 5, 5, 5,   5, 5, 5, 5, 5, 5, 5, 5,
       6, 7, 7, 7,  7, 7, 7, 7,   7, 7, 7, 7, 7, 8, 7, 7,
      10, 9, 9, 9, 11, 4, 4, 4,   4, 4, 4, 4, 4, 4, 4, 4,

      // The second part of the table maps a state to a new state when adding a
      // transition.
       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
      12,  0,  0,  0,  0, 24, 36, 48, 60, 72, 84, 96,
       0, 12, 12, 12,  0,  0,  0,  0,  0,  0,  0,  0,
       0,  0,  0, 24,  0,  0,  0,  0,  0,  0,  0,  0,
       0, 24, 24, 24,  0,  0,  0,  0,  0,  0,  0,  0,
       0, 24, 24,  0,  0,  0,  0,  0,  0,  0,  0,  0,
       0, 48, 48, 48,  0,  0,  0,  0,  0,  0,  0,  0,
       0,  0, 48, 48,  0,  0,  0,  0,  0,  0,  0,  0,
       0, 48,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,

      // The third part maps the current transition to a mask that needs to
      // apply to the byte.
      0x7F, 0x3F, 0x3F, 0x3F, 0x00, 0x1F, 0x0F, 0x0F, 0x0F, 0x07, 0x07, 0x07,
  };
  uint8_t type = kUtf8Data[byte];

  *state = kUtf8Data[256 + *state + type];
  return (codep << 6) | (byte & kUtf8Data[364 + type]);
}

/**
 * Shifts the "normal" characters from there current location in the string buffer
 * to the current decoded index. This is necessary because, as we decode percent-encoded
 * values, we take up less space.
 */
template <typename SourceChar, typename DestChar>
static DestChar *ShiftChars(DestChar *dest, SourceChar *src, size_t n) {
  CopyChars(dest, src, n);
  return dest + n;
}

template <typename SourceChar, typename DestChar>
static MaybeHandle<String> DecodeSlow(Isolate* isolate, Handle<String> uri,
                                      bool is_uri, bool two_byte, size_t size) {
  // TODO COMMENTS
  Vector<const SourceChar> content = uri->GetCharVector<SourceChar>();
  DestChar *decoded = NewArray<DestChar>(size);

  StringSearch<uint8_t, SourceChar> search(isolate, STATIC_CHAR_VECTOR("%"));

  const SourceChar *begin = content.begin();

  int length = content.length();

  // The current octet that we are decoding.
  int k = search.Search(content, 0);

  // The position of the first octet in this series.
  int start_of_octets = k;

  // The position of the first character after the last decoded octet.
  // Everything from this point on (up to the start of the escape sequence)
  // will need to be moved leftwards to the insertion point if a new valid
  // octet series is encountered.
  int start_of_chars = 0;

  // The current "insertion" pointer, where we move our decoded and normal
  // characters.
  DestChar *insertion = decoded;

  // The current octet series' accumulated code point.
  uc32 codepoint = 0;

  // The state of the decoding FSM.
  uint8_t state = kUtf8Accept;

  // Normally, we'd have to check for k < length - 2, but we've already
  // done that check.
  while (k >= 0) {
    // The character at k is always a '%', guaranteed by the Find.
    // So, interpret the hex chars as a single value.
    uint8_t high = HexCharToInt(content[k + 1], 4);
    uint8_t low = HexCharToInt(content[k + 2], 0);

    // If either is not a valid hex char, the result will be `255`, making
    // their OR 255, and making the FSM reject (255 is always an invalid byte).
    // Otherwise, this will transition `state` into the next.
    codepoint = Utf8Decode(codepoint, high | low, &state);

    switch (state) {
      case kUtf8Accept:
        // A full codepoint has been found!

        // If we this codepoint is a reserved char (decodeURI), pretend we
        // never saw it.
        if (is_uri && IsReservedPredicate(codepoint)) {
          // Find our next '%', after this sequence
          k = search.Search(content, k + 3);
        } else {
          // We first need to shift any characters leftwards, if any codepoints
          // have been found previously (since they decode into fewer
          // characters).
          insertion = ShiftChars(insertion, begin + start_of_chars,
              start_of_octets - start_of_chars);

          // Push either a single character, or a surrogate character.
          if (codepoint <= static_cast<uc32>(
                unibrow::Utf16::kMaxNonSurrogateCharCode)) {
            *insertion++ = codepoint;
          } else {
            *insertion++ = unibrow::Utf16::LeadSurrogate(codepoint);
            *insertion++ = unibrow::Utf16::TrailSurrogate(codepoint);
          }

          // Now, the first char after this escape is the next start_of_chars.
          start_of_chars = k + 3;

          // Find our next '%', after this sequence
          k = search.Search(content, start_of_chars);
        }

        start_of_octets = k;
        codepoint = 0;
        break;

      default:
        // We're in the middle of a multi-octet series.
        // Move to the next octet.
        k += 3;
        // If we find a '%', break out and continue with the loop.
        // Otherwise, we've encountered an invalid octet series, and need to
        // reject.
        if (k < length && content[k] == '%') {
          break;
        }

      // Intentional fall-through
      case kUtf8Reject:
        DeleteArray(decoded);
        THROW_NEW_ERROR(isolate, NewURIError(), String);
    }
  }

  // Finally, if there are any normal characters after the last decoded octets,
  // we need to shift them leftwards.
  ShiftChars(insertion, begin + start_of_chars,
      length - start_of_chars);

  if (two_byte) {
    return isolate->factory()->NewStringFromTwoByte(Vector<const uc16>(
          reinterpret_cast<uc16 *>(decoded), size));
  } else {
    return isolate->factory()->NewStringFromOneByte(Vector<const uint8_t>(
          reinterpret_cast<uint8_t *>(decoded), size));
  }
}

template <typename Char>
static MaybeHandle<String> DecodePrivate(Isolate* isolate, Handle<String> uri,
                                         bool is_uri, bool two_byte) {
  size_t size;

  {
    DisallowHeapAllocation no_allocation;
    StringSearch<uint8_t, Char> search(isolate, STATIC_CHAR_VECTOR("%"));

    Vector<const Char> content = uri->GetCharVector<Char>();
    size = content.size();

    int index = search.Search(content, 0);

    // If there are no percents to decode, we can early return.
    if (index < 0) {
      return uri;
    }

    int length = content.length();
    while (index >= 0 && index < length - 2) {
      uint8_t codepoint = HexCharToInt(content[index + 1], 4) |
        HexCharToInt(content[index + 2]);

      // If the lead codepoint of 0xC4 or greater is guaranteed to give a two
      // byte character (if the escapes are valid).

      if (is_uri && IsReservedPredicate(codepoint)) {
        index += 3;
      } else {
        if (codepoint < 0x80) {
          index += 3;
          size -= 2;
        } else if (codepoint < 0xE0) {
          two_byte = two_byte || codepoint > 0xC4;
          index += 6;
          size -= 5;
        } else if (codepoint < 0xF0) {
          two_byte = true;
          index += 9;
          size -= 8;
        } else {
          two_byte = true;
          index += 12;
          size -= 10;
        }
      }

      index = search.Search(content, index);
    }

    // '%' in the last 2 chars is an easily detectable early error,
    // and it cleans up logic further down.
    if (index >= 0) {
      THROW_NEW_ERROR(isolate, NewURIError(), String);
    }

    // If we "decoded" only reserved chars, we can early return.
    if (size == content.size()) {
      return uri;
    }
  }

  if (two_byte) {
    return DecodeSlow<Char, uc16>(isolate, uri, is_uri, two_byte, size);
  } else {
    return DecodeSlow<Char, uint8_t>(isolate, uri, is_uri, two_byte, size);
  }
}

}  // anonymous namespace

MaybeHandle<String> Uri::Decode(Isolate* isolate, Handle<String> uri,
                                bool is_uri) {
  uri = String::Flatten(uri);
  return uri->IsOneByteRepresentationUnderneath()
             ? DecodePrivate<uint8_t>(isolate, uri, is_uri, false)
             : DecodePrivate<uc16>(isolate, uri, is_uri, true);
}

namespace {  // anonymous namespace for EncodeURI helper functions
bool IsUnescapePredicateInUriComponent(uc16 c) {
  if (IsAlphaNumeric(c)) {
    return true;
  }

  switch (c) {
    case '!':
    case '\'':
    case '(':
    case ')':
    case '*':
    case '-':
    case '.':
    case '_':
    case '~':
      return true;
    default:
      return false;
  }
}

bool IsUriSeparator(uc16 c) {
  switch (c) {
    case '#':
    case '$':
    case '&':
    case '+':
    case ',':
    case '/':
    case ':':
    case ';':
    case '=':
    case '?':
    case '@':
      return true;
    default:
      return false;
  }
}

void AddEncodedOctetToBuffer(uint8_t octet, std::vector<uint8_t>* buffer) {
  buffer->push_back('%');
  buffer->push_back(HexCharOfValue(octet >> 4));
  buffer->push_back(HexCharOfValue(octet & 0x0F));
}

void EncodeSingle(uc16 c, std::vector<uint8_t>* buffer) {
  char s[4] = {};
  int number_of_bytes;
  number_of_bytes =
      unibrow::Utf8::Encode(s, c, unibrow::Utf16::kNoPreviousCharacter, false);
  for (int k = 0; k < number_of_bytes; k++) {
    AddEncodedOctetToBuffer(s[k], buffer);
  }
}

void EncodePair(uc16 cc1, uc16 cc2, std::vector<uint8_t>* buffer) {
  char s[4] = {};
  int number_of_bytes =
      unibrow::Utf8::Encode(s, unibrow::Utf16::CombineSurrogatePair(cc1, cc2),
                            unibrow::Utf16::kNoPreviousCharacter, false);
  for (int k = 0; k < number_of_bytes; k++) {
    AddEncodedOctetToBuffer(s[k], buffer);
  }
}

}  // anonymous namespace

MaybeHandle<String> Uri::Encode(Isolate* isolate, Handle<String> uri,
                                bool is_uri) {
  uri = String::Flatten(uri);
  int uri_length = uri->length();
  std::vector<uint8_t> buffer;
  buffer.reserve(uri_length);

  {
    DisallowHeapAllocation no_gc;
    String::FlatContent uri_content = uri->GetFlatContent();

    for (int k = 0; k < uri_length; k++) {
      uc16 cc1 = uri_content.Get(k);
      if (unibrow::Utf16::IsLeadSurrogate(cc1)) {
        k++;
        if (k < uri_length) {
          uc16 cc2 = uri->Get(k);
          if (unibrow::Utf16::IsTrailSurrogate(cc2)) {
            EncodePair(cc1, cc2, &buffer);
            continue;
          }
        }
      } else if (!unibrow::Utf16::IsTrailSurrogate(cc1)) {
        if (IsUnescapePredicateInUriComponent(cc1) ||
            (is_uri && IsUriSeparator(cc1))) {
          buffer.push_back(cc1);
        } else {
          EncodeSingle(cc1, &buffer);
        }
        continue;
      }

      AllowHeapAllocation allocate_error_and_return;
      THROW_NEW_ERROR(isolate, NewURIError(), String);
    }
  }

  return isolate->factory()->NewStringFromOneByte(
      Vector<const uint8_t>(buffer.data(), static_cast<int>(buffer.size())));
}

namespace {  // Anonymous namespace for Escape and Unescape

int TwoDigitHex(uc16 character1, uc16 character2) {
  if (character1 > 'f') return -1;
  int high = HexValue(character1);
  if (high == -1) return -1;
  if (character2 > 'f') return -1;
  int low = HexValue(character2);
  if (low == -1) return -1;
  return (high << 4) + low;
}

template <typename Char>
int UnescapeChar(Vector<const Char> vector, int i, int length, int* step) {
  uint16_t character = vector[i];
  int32_t hi = 0;
  int32_t lo = 0;
  if (character == '%' && i <= length - 6 && vector[i + 1] == 'u' &&
      (hi = TwoDigitHex(vector[i + 2], vector[i + 3])) > -1 &&
      (lo = TwoDigitHex(vector[i + 4], vector[i + 5])) > -1) {
    *step = 6;
    return (hi << 8) + lo;
  } else if (character == '%' && i <= length - 3 &&
             (lo = TwoDigitHex(vector[i + 1], vector[i + 2])) > -1) {
    *step = 3;
    return lo;
  } else {
    *step = 1;
    return character;
  }
}

template <typename Char>
MaybeHandle<String> UnescapeSlow(Isolate* isolate, Handle<String> string,
                                 int start_index) {
  bool one_byte = true;
  int length = string->length();

  int unescaped_length = 0;
  {
    DisallowHeapAllocation no_allocation;
    Vector<const Char> vector = string->GetCharVector<Char>();
    for (int i = start_index; i < length; unescaped_length++) {
      int step;
      if (UnescapeChar(vector, i, length, &step) >
          String::kMaxOneByteCharCode) {
        one_byte = false;
      }
      i += step;
    }
  }

  DCHECK(start_index < length);
  Handle<String> first_part =
      isolate->factory()->NewProperSubString(string, 0, start_index);

  int dest_position = 0;
  Handle<String> second_part;
  DCHECK_LE(unescaped_length, String::kMaxLength);
  if (one_byte) {
    Handle<SeqOneByteString> dest = isolate->factory()
                                        ->NewRawOneByteString(unescaped_length)
                                        .ToHandleChecked();
    DisallowHeapAllocation no_allocation;
    Vector<const Char> vector = string->GetCharVector<Char>();
    for (int i = start_index; i < length; dest_position++) {
      int step;
      dest->SeqOneByteStringSet(dest_position,
                                UnescapeChar(vector, i, length, &step));
      i += step;
    }
    second_part = dest;
  } else {
    Handle<SeqTwoByteString> dest = isolate->factory()
                                        ->NewRawTwoByteString(unescaped_length)
                                        .ToHandleChecked();
    DisallowHeapAllocation no_allocation;
    Vector<const Char> vector = string->GetCharVector<Char>();
    for (int i = start_index; i < length; dest_position++) {
      int step;
      dest->SeqTwoByteStringSet(dest_position,
                                UnescapeChar(vector, i, length, &step));
      i += step;
    }
    second_part = dest;
  }
  return isolate->factory()->NewConsString(first_part, second_part);
}

bool IsNotEscaped(uint16_t c) {
  if (IsAlphaNumeric(c)) {
    return true;
  }
  //  @*_+-./
  switch (c) {
    case '@':
    case '*':
    case '_':
    case '+':
    case '-':
    case '.':
    case '/':
      return true;
    default:
      return false;
  }
}

template <typename Char>
static MaybeHandle<String> UnescapePrivate(Isolate* isolate,
                                           Handle<String> source) {
  int index;
  {
    DisallowHeapAllocation no_allocation;
    StringSearch<uint8_t, Char> search(isolate, STATIC_CHAR_VECTOR("%"));
    index = search.Search(source->GetCharVector<Char>(), 0);
    if (index < 0) return source;
  }
  return UnescapeSlow<Char>(isolate, source, index);
}

template <typename Char>
static MaybeHandle<String> EscapePrivate(Isolate* isolate,
                                         Handle<String> string) {
  DCHECK(string->IsFlat());
  int escaped_length = 0;
  int length = string->length();

  {
    DisallowHeapAllocation no_allocation;
    Vector<const Char> vector = string->GetCharVector<Char>();
    for (int i = 0; i < length; i++) {
      uint16_t c = vector[i];
      if (c >= 256) {
        escaped_length += 6;
      } else if (IsNotEscaped(c)) {
        escaped_length++;
      } else {
        escaped_length += 3;
      }

      // We don't allow strings that are longer than a maximal length.
      DCHECK_LT(String::kMaxLength, 0x7fffffff - 6);   // Cannot overflow.
      if (escaped_length > String::kMaxLength) break;  // Provoke exception.
    }
  }

  // No length change implies no change.  Return original string if no change.
  if (escaped_length == length) return string;

  Handle<SeqOneByteString> dest;
  ASSIGN_RETURN_ON_EXCEPTION(
      isolate, dest, isolate->factory()->NewRawOneByteString(escaped_length),
      String);
  int dest_position = 0;

  {
    DisallowHeapAllocation no_allocation;
    Vector<const Char> vector = string->GetCharVector<Char>();
    for (int i = 0; i < length; i++) {
      uint16_t c = vector[i];
      if (c >= 256) {
        dest->SeqOneByteStringSet(dest_position, '%');
        dest->SeqOneByteStringSet(dest_position + 1, 'u');
        dest->SeqOneByteStringSet(dest_position + 2, HexCharOfValue(c >> 12));
        dest->SeqOneByteStringSet(dest_position + 3,
                                  HexCharOfValue((c >> 8) & 0xf));
        dest->SeqOneByteStringSet(dest_position + 4,
                                  HexCharOfValue((c >> 4) & 0xf));
        dest->SeqOneByteStringSet(dest_position + 5, HexCharOfValue(c & 0xf));
        dest_position += 6;
      } else if (IsNotEscaped(c)) {
        dest->SeqOneByteStringSet(dest_position, c);
        dest_position++;
      } else {
        dest->SeqOneByteStringSet(dest_position, '%');
        dest->SeqOneByteStringSet(dest_position + 1, HexCharOfValue(c >> 4));
        dest->SeqOneByteStringSet(dest_position + 2, HexCharOfValue(c & 0xf));
        dest_position += 3;
      }
    }
  }

  return dest;
}

}  // Anonymous namespace

MaybeHandle<String> Uri::Escape(Isolate* isolate, Handle<String> string) {
  string = String::Flatten(string);
  return string->IsOneByteRepresentationUnderneath()
             ? EscapePrivate<uint8_t>(isolate, string)
             : EscapePrivate<uc16>(isolate, string);
}

MaybeHandle<String> Uri::Unescape(Isolate* isolate, Handle<String> string) {
  string = String::Flatten(string);
  return string->IsOneByteRepresentationUnderneath()
             ? UnescapePrivate<uint8_t>(isolate, string)
             : UnescapePrivate<uc16>(isolate, string);
}

}  // namespace internal
}  // namespace v8
