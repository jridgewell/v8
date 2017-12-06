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

  unbuffered_start_ = stream + bytes_read;
  unbuffered_length_ = stream_length - bytes_read;
  last_byte_of_buffer_unused_ = bytes_read < buffer_length;

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
  while (data_length != 0) {
    size_t cursor = 0;
    uint32_t character = Utf8::ValueOf(stream, stream_length, &cursor);
    // There's a total lack of bounds checking for stream
    // as it was already done in Reset.
    stream += cursor;
    DCHECK(stream_length >= cursor);
    stream_length -= cursor;
    if (character > unibrow::Utf16::kMaxNonSurrogateCharCode) {
      *data++ = Utf16::LeadSurrogate(character);
      *data++ = Utf16::TrailSurrogate(character);
      DCHECK_GT(data_length, 1);
      data_length -= 2;
    } else {
      *data++ = character;
      data_length -= 1;
    }
  }
}

}  // namespace unibrow
