#ifndef INCLUDE_GUARD_HASH_TABLE_H
#define INCLUDE_GUARD_HASH_TABLE_H

#include "memory.h"


template<typename KeyType>
using HashTableHashFunc = u32(KeyType key);

template<typename KeyType, typename ValueType, HashTableHashFunc<KeyType> *hash_func>
struct HashTable {
	struct Entry {
		KeyType   key;
		ValueType value;
		u32       hash;
	};

	Allocator allocator;

	Entry *entries;
	s32 buckets;
	s32 alloc;
	s32 max_load;
};


template<typename KeyType, typename ValueType, HashTableHashFunc<KeyType> *hash_func>
void init(HashTable<KeyType, ValueType, hash_func> *table, s32 size) {
	typedef typename HashTable<KeyType, ValueType, hash_func>::Entry Entry;
	r32 const default_max_load = 0.6f;

	assert((size & (size - 1)) == 0);
	assert(table->allocator.allocate == 0 && table->alloc == 0);
	table->allocator = get_memory_context()->allocator;

	table->entries  = (Entry*)allocate_memory(table->allocator, size * sizeof(Entry));
	table->buckets  = 0;
	table->alloc    = size;
	table->max_load = size * default_max_load;
}

template<typename KeyType, typename ValueType, HashTableHashFunc<KeyType> *hash_func>
ValueType *insert(HashTable<KeyType, ValueType, hash_func> *table, KeyType key, ValueType value) {
	typedef typename HashTable<KeyType, ValueType, hash_func>::Entry Entry;

	if (table->alloc == 0) {
		u32 const default_size = 128;
		init(table, default_size);
	}

	if (table->buckets == table->max_load) {
		grow(table);
	}

	u32 hash = hash_func(key);
	if (hash == 0) hash += 1;

	u32 mask = table->alloc - 1;
	s32 bucket = hash & mask;

	for (;;) {
		Entry *entry = &table->entries[bucket];
		if (entry->hash == 0) {
			entry->hash  = hash;
			entry->key   = key;
			entry->value = value;

			table->buckets += 1;

			return &entry->value;
		}

		bucket = (bucket + 1) & mask;
	}

	return 0;
}

template<typename KeyType, typename ValueType, HashTableHashFunc<KeyType> *hash_func>
ValueType *find(HashTable<KeyType, ValueType, hash_func> *table, KeyType key) {
	typedef typename HashTable<KeyType, ValueType, hash_func>::Entry Entry;

	if (table->buckets == 0) return 0;
	
	u32 hash = hash_func(key);
	if (hash == 0) hash += 1;

	u32 mask = table->alloc - 1;
	s32 bucket = hash & mask;

	for (;;) {
		Entry *entry = &table->entries[bucket];
		if (entry->hash == 0) break;

		if (entry->hash == hash) {
			if (equal(entry->key, key)) {
				return &entry->value;
			}
		}

		bucket = (bucket + 1) & mask;
	}

	return 0;
}

template<typename KeyType, typename ValueType, HashTableHashFunc<KeyType> *hash_func>
void grow(HashTable<KeyType, ValueType, hash_func> *table) {
	assert(table->alloc);

	typedef typename HashTable<KeyType, ValueType, hash_func>::Entry Entry;
	u32 const GrowFactor = 2;

	HashTable<KeyType, ValueType, hash_func> new_table = {};
	init(&new_table, table->alloc * GrowFactor);

	for (s32 i = 0; i < table->alloc; i += 1) {
		if (table->entries[i].hash != 0) {
			insert(&new_table, table->entries[i].key, table->entries[i].value);
		}
	}

	destroy(table);
	*table = new_table;
}

template<typename KeyType, typename ValueType, HashTableHashFunc<KeyType> *hash_func>
void destroy(HashTable<KeyType, ValueType, hash_func> *table) {
	deallocate_memory(table->allocator, table->entries);

	INIT_STRUCT(table);
}

#endif // INCLUDE_GUARD_HASH_TABLE_H

