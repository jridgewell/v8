// Copyright 2014 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.


#include "src/unicode-inl.h"
#include "src/unicode-decoder.h"
#include <stdio.h>
#include <stdlib.h>

namespace unibrow {

void Utf8DecoderBase::Reset(uint16_t* buffer, size_t buffer_length,
                            const uint8_t* stream, size_t stream_length) {
  size_t utf16_length = 0;
  size_t cursor = 0;
  size_t bytes_read = 0;
  uint32_t utf8_buffer = 0;
  Utf8::State state = Utf8::State::kAccept;

  // Loop until stream is read, writing to buffer as long as buffer has space.
  while (cursor < stream_length) {
    uint32_t c =
        Utf8::ValueOfIncremental(stream[cursor], &cursor, &state, &utf8_buffer);
    if (c == Utf8::kIncomplete) continue;

    bool is_two_characters = c > Utf16::kMaxNonSurrogateCharCode;
    utf16_length += is_two_characters ? 2 : 1;
    if (utf16_length > buffer_length) break;

    bytes_read = cursor;
    if (is_two_characters) {
      *buffer++ = Utf16::LeadSurrogate(c);
      *buffer++ = Utf16::TrailSurrogate(c);
    } else {
      *buffer++ = c;
    }
  }

  bytes_read_ = bytes_read;
  unbuffered_start_ = stream + bytes_read;
  unbuffered_length_ = stream_length - bytes_read;

  // Now that writing to buffer is done, we just need to calculate utf16_length
  while (cursor < stream_length) {
    uint32_t c =
        Utf8::ValueOfIncremental(stream[cursor], &cursor, &state, &utf8_buffer);
    if (c == Utf8::kIncomplete) continue;
    bool is_two_characters = c > Utf16::kMaxNonSurrogateCharCode;
    utf16_length += is_two_characters ? 2 : 1;
  }

  uint32_t end = Utf8::ValueOfIncrementalFinish(&state);
  if (end) {
    DCHECK_LT(end, Utf16::kMaxNonSurrogateCharCode);
    utf16_length++;
  }

  utf16_length_ = utf16_length;
}


void Utf8DecoderBase::WriteUtf16Slow(const uint8_t* stream,
                                     size_t stream_length, uint16_t* data,
                                     size_t data_length) {
  size_t cursor = 0;
  size_t written = 0;
  uint32_t buffer = 0;
  Utf8::State state = Utf8::State::kAccept;
  while (cursor < stream_length) {
    uint32_t c = Utf8::ValueOfIncremental(stream[cursor], &cursor, &state, &buffer);
    if (c == Utf8::kIncomplete) continue;

    // There's a total lack of bounds checking for stream
    // as it was already done in Reset.
    if (c > Utf16::kMaxNonSurrogateCharCode) {
      written += 2;
      DCHECK_GE(data_length, written);
      *data++ = Utf16::LeadSurrogate(c);
      *data++ = Utf16::TrailSurrogate(c);
    } else {
      written++;
      *data++ = c;
    }
  }
}

}  // namespace unibrow
