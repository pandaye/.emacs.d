#!/usr/bin/env python3
"""Return the PySBD sentence containing a character offset."""

import json
import sys

import pysbd


def compact(text):
    return " ".join(text.split())


def main():
    payload = json.load(sys.stdin)
    text = payload["text"]
    offset = int(payload["offset"])
    language = payload.get("language", "en")

    segmenter = pysbd.Segmenter(language=language, clean=False)
    cursor = 0
    fallback = compact(text)

    for sentence in segmenter.segment(text):
        start = text.find(sentence, cursor)
        if start < 0:
            continue

        end = start + len(sentence)
        if start <= offset <= end:
            print(json.dumps({"sentence": compact(sentence)}, ensure_ascii=False))
            return 0

        cursor = end

    print(json.dumps({"sentence": fallback}, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
