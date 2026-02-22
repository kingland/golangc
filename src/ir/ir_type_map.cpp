#include "ir/ir_type_map.hpp"

namespace golangc {
namespace ir {

IRTypeMap::IRTypeMap() {
    void_ = make_type(IRTypeKind::Void);
    i1_   = make_type(IRTypeKind::I1);
    i8_   = make_type(IRTypeKind::I8);
    i16_  = make_type(IRTypeKind::I16);
    i32_  = make_type(IRTypeKind::I32);
    i64_  = make_type(IRTypeKind::I64);
    f32_  = make_type(IRTypeKind::F32);
    f64_  = make_type(IRTypeKind::F64);
    ptr_  = make_type(IRTypeKind::Ptr);

    // string = {ptr, i64} — pointer to data + length
    string_ = make_struct_type({ptr_, i64_}, "string");

    // slice = {ptr, i64, i64} — pointer to data + length + capacity
    slice_ = make_struct_type({ptr_, i64_, i64_}, "slice");

    // interface = {ptr, ptr} — type descriptor + data pointer
    iface_ = make_struct_type({ptr_, ptr_}, "interface");
}

IRType* IRTypeMap::make_type(IRTypeKind kind) {
    auto t = std::make_unique<IRType>(kind);
    auto* ptr = t.get();
    owned_types_.push_back(std::move(t));
    return ptr;
}

IRType* IRTypeMap::make_struct_type(std::vector<IRType*> fields, std::string name) {
    auto t = std::make_unique<IRType>(IRTypeKind::Struct);
    t->fields = std::move(fields);
    t->name = std::move(name);
    auto* ptr = t.get();
    owned_types_.push_back(std::move(t));
    return ptr;
}

IRType* IRTypeMap::make_array_type(IRType* elem, int64_t count) {
    auto t = std::make_unique<IRType>(IRTypeKind::Array);
    t->element = elem;
    t->count = count;
    auto* ptr = t.get();
    owned_types_.push_back(std::move(t));
    return ptr;
}

IRType* IRTypeMap::make_func_type(IRType* ret, std::vector<IRType*> params) {
    auto t = std::make_unique<IRType>(IRTypeKind::Func);
    t->return_type = ret;
    t->param_types = std::move(params);
    auto* ptr = t.get();
    owned_types_.push_back(std::move(t));
    return ptr;
}

IRType* IRTypeMap::map_basic_type(sema::BasicKind kind) {
    switch (kind) {
        case sema::BasicKind::Bool:
        case sema::BasicKind::UntypedBool:
            return i1_;
        case sema::BasicKind::Int8:
        case sema::BasicKind::Uint8:
            return i8_;
        case sema::BasicKind::Int16:
        case sema::BasicKind::Uint16:
            return i16_;
        case sema::BasicKind::Int32:
        case sema::BasicKind::Uint32:
        case sema::BasicKind::UntypedRune:
            return i32_;
        case sema::BasicKind::Int:
        case sema::BasicKind::Int64:
        case sema::BasicKind::Uint:
        case sema::BasicKind::Uint64:
        case sema::BasicKind::Uintptr:
        case sema::BasicKind::UntypedInt:
            return i64_;
        case sema::BasicKind::Float32:
            return f32_;
        case sema::BasicKind::Float64:
        case sema::BasicKind::UntypedFloat:
            return f64_;
        case sema::BasicKind::String:
        case sema::BasicKind::UntypedString:
            return string_;
        case sema::BasicKind::UntypedNil:
            return ptr_;
        case sema::BasicKind::Complex64:
        case sema::BasicKind::Complex128:
        case sema::BasicKind::UntypedComplex:
            // Complex types: represent as struct of two floats
            return make_struct_type({f64_, f64_}, "complex128");
        case sema::BasicKind::Invalid:
            return void_;
    }
    return void_;
}

IRType* IRTypeMap::map_type(sema::Type* t) {
    if (!t) return void_;

    // Check cache
    auto it = cache_.find(t);
    if (it != cache_.end()) return it->second;

    IRType* result = nullptr;

    switch (t->kind) {
        case sema::TypeKind::Invalid:
            result = void_;
            break;

        case sema::TypeKind::Basic:
            result = map_basic_type(t->basic);
            break;

        case sema::TypeKind::Pointer:
            result = ptr_;
            break;

        case sema::TypeKind::Array: {
            auto* elem = map_type(t->array.element);
            result = make_array_type(elem, t->array.length);
            break;
        }

        case sema::TypeKind::Slice:
            result = slice_;
            break;

        case sema::TypeKind::Map:
            result = ptr_;  // Maps are pointer-like at runtime
            break;

        case sema::TypeKind::Chan:
            result = ptr_;  // Channels are pointer-like at runtime
            break;

        case sema::TypeKind::Func: {
            // Map function type
            IRType* ret = void_;
            std::vector<IRType*> params;

            if (t->func) {
                if (!t->func->results.empty()) {
                    if (t->func->results.size() == 1) {
                        ret = map_type(t->func->results[0].type);
                    } else {
                        // Multiple returns: create a tuple struct
                        std::vector<IRType*> ret_fields;
                        for (const auto& r : t->func->results) {
                            ret_fields.push_back(map_type(r.type));
                        }
                        ret = make_struct_type(std::move(ret_fields));
                    }
                }
                for (const auto& p : t->func->params) {
                    params.push_back(map_type(p.type));
                }
            }
            result = make_func_type(ret, std::move(params));
            break;
        }

        case sema::TypeKind::Struct: {
            if (t->struct_) {
                std::vector<IRType*> fields;
                for (const auto& f : t->struct_->fields) {
                    fields.push_back(map_type(f.type));
                }
                result = make_struct_type(std::move(fields));
            } else {
                result = make_struct_type({});
            }
            break;
        }

        case sema::TypeKind::Interface:
            result = iface_;
            break;

        case sema::TypeKind::Named:
            // Map through to underlying type
            if (t->named && t->named->underlying) {
                result = map_type(t->named->underlying);
            } else {
                result = void_;
            }
            break;

        case sema::TypeKind::Tuple: {
            if (t->tuple) {
                std::vector<IRType*> fields;
                for (auto* elem : t->tuple->types) {
                    fields.push_back(map_type(elem));
                }
                result = make_struct_type(std::move(fields));
            } else {
                result = void_;
            }
            break;
        }
    }

    cache_[t] = result;
    return result;
}

IRType* IRTypeMap::make_tuple_type(std::vector<IRType*> fields) {
    return make_struct_type(std::move(fields));
}

int64_t IRTypeMap::type_size(const IRType* t) {
    if (!t) return 8;
    switch (t->kind) {
        case IRTypeKind::Void: return 0;
        case IRTypeKind::I1:
        case IRTypeKind::I8:  return 1;
        case IRTypeKind::I16: return 2;
        case IRTypeKind::I32:
        case IRTypeKind::F32: return 4;
        case IRTypeKind::I64:
        case IRTypeKind::F64:
        case IRTypeKind::Ptr: return 8;
        case IRTypeKind::Struct: {
            int64_t total = 0;
            for (auto* f : t->fields) total += type_size(f);
            return total;
        }
        case IRTypeKind::Array:
            return (t->element ? type_size(t->element) : 8) * t->count;
        default: return 8;
    }
}

} // namespace ir
} // namespace golangc
