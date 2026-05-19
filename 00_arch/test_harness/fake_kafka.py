"""
Fake kafka module for the test harness — no real Kafka broker needed.
Call install(consumer_instance) to inject into sys.modules before exec'ing the script.
"""
import json
import struct
import sys
from collections import namedtuple

TopicPartition    = namedtuple("TopicPartition",    ["topic", "partition"])
OffsetAndMetadata = namedtuple("OffsetAndMetadata", ["offset", "metadata"])
OffsetAndTimestamp = namedtuple("OffsetAndTimestamp", ["offset", "timestamp"])

FAKE_SCHEMA_ID = 42  # returned by fake schema registry; embedded in each FakeMessage


class FakeMessage:
    """Mimics a kafka-python ConsumerRecord."""

    def __init__(self, topic, partition_idx, offset, timestamp_ms, msg_index):
        self.topic     = topic
        self.partition = partition_idx
        self.offset    = offset
        self.timestamp = timestamp_ms
        self.key       = None
        self.headers   = []
        # wire format the script expects:
        #   byte 0      : 0x00 (magic)
        #   bytes 1-4   : schema_id big-endian
        #   bytes 5-8   : msg_index big-endian  ← decoded by mock schemaless_reader
        self.value = (
            b'\x00'
            + struct.pack('>I', FAKE_SCHEMA_ID)
            + struct.pack('>I', msg_index)
        )


class FakeKafkaConsumer:
    """
    Replays a fixed list of messages from a single partition.

    msg_dicts      : list of Python dicts (post-deserialization content)
    topic          : topic name string
    start_ts_ms    : timestamp of message 0 in milliseconds
    msg_interval_ms: milliseconds between consecutive messages
    """

    def __init__(self, msg_dicts, topic, start_ts_ms, msg_interval_ms=60_000, data_file=None):
        self._topic     = topic
        self._msgs      = list(msg_dicts)   # mutable copy — reloaded in-place on each iter
        self._n         = len(self._msgs)
        self._start_ms  = start_ts_ms
        self._interval  = msg_interval_ms
        self._data_file = data_file
        self._assigned  = []
        self._seek_pos  = {}   # {partition_int: next_offset}
        self._committed = {}   # {TopicPartition: OffsetAndMetadata}

    def _reload_if_needed(self):
        if not self._data_file:
            return
        with open(self._data_file) as f:
            new_msgs = [json.loads(line) for line in f if line.strip()]
        self._msgs[:] = new_msgs   # in-place so reader's reference stays valid
        self._n = len(self._msgs)

    # ── connection / partition setup ──────────────────────────────────────────

    def bootstrap_connected(self):
        return True

    def partitions_for_topic(self, topic):
        return {0}

    def assign(self, partitions):
        self._assigned = list(partitions)

    def assignment(self):
        return set(self._assigned)

    # ── offset / timestamp lookup ─────────────────────────────────────────────

    def _ts_of(self, i):
        return self._start_ms + i * self._interval

    def offsets_for_times(self, partition_ts_dict):
        result = {}
        for tp, ts_ms in partition_ts_dict.items():
            found = None
            for i in range(self._n):
                if self._ts_of(i) >= ts_ms:
                    found = OffsetAndTimestamp(i, self._ts_of(i))
                    break
            result[tp] = found
        return result

    def end_offsets(self, partitions):
        self._reload_if_needed()
        return {tp: self._n for tp in partitions}

    def committed(self, partition, metadata=False):
        return self._committed.get(partition)

    def commit(self, offsets):
        for tp, oam in offsets.items():
            self._committed[tp] = oam

    # ── seek / position ───────────────────────────────────────────────────────

    def seek(self, partition, offset):
        self._seek_pos[partition.partition] = offset

    def position(self, partition):
        return self._seek_pos.get(partition.partition, 0)

    # ── iteration — collect pass (for msg in consumer) ────────────────────────

    def __iter__(self):
        self._reload_if_needed()
        p = self._assigned[0].partition if self._assigned else 0
        start = self._seek_pos.get(p, 0)
        for i in range(start, self._n):
            self._seek_pos[p] = i + 1
            yield FakeMessage(self._topic, p, i, self._ts_of(i), i)

    # ── poll — write pass ─────────────────────────────────────────────────────

    def poll(self, timeout_ms=500, max_records=100):
        self._reload_if_needed()
        if not self._assigned:
            return {}
        tp = self._assigned[0]
        p  = tp.partition
        start = self._seek_pos.get(p, 0)
        if start >= self._n:
            return {}
        end = min(start + max_records, self._n)
        msgs = [FakeMessage(self._topic, p, i, self._ts_of(i), i)
                for i in range(start, end)]
        self._seek_pos[p] = end
        return {tp: msgs}


# ── module injection ──────────────────────────────────────────────────────────

def install(consumer_instance):
    """Replace sys.modules['kafka'] with a fake that returns consumer_instance."""
    _consumer = consumer_instance

    class _KafkaConsumer:
        def __new__(cls, *args, **kwargs):
            return _consumer

    mod = type(sys)("kafka")
    mod.KafkaConsumer    = _KafkaConsumer
    mod.TopicPartition   = TopicPartition
    mod.OffsetAndMetadata = OffsetAndMetadata

    sys.modules["kafka"]        = mod
    sys.modules["kafka.errors"] = type(sys)("kafka.errors")
    return mod
