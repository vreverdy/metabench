// =============================== METABENCH ================================ //
// Project:         MetaBench
// Name:            metabench.cpp
// Description:     A C++ Compiler Benchmark
// Creator:         Vincent Reverdy
// Contributor(s):  Vincent Reverdy [2022-]
// License:         CC0 1.0 Universal - Public Domain Dedication 
// ========================================================================== //
// This file is a standalone file. It includes the entire metabench library. You
// can just copy it anywhere, include it in your project, and use it to 
// benchmark compilers.
// ========================================================================== //
#ifndef _METABENCH_HPP_INCLUDED
#define _METABENCH_HPP_INCLUDED
// ========================================================================== //



// ================================ PREAMBLE ================================ //
// C++ standard library
#include <tuple>
#include <utility>
#include <typeinfo>
#include <type_traits>
// Namespace
namespace metabench {
// ========================================================================== //



// ======================= GENERIC PACK TYPE INTERFACE ====================== //
// The interface to get the size of a pack type
template <class T>
struct pack_size;

// Returns the size of pack type
template <class T>
inline constexpr std::size_t pack_size_v = pack_size<T>::value;
// -------------------------------------------------------------------------- //
// The interface to get the element of the pack type at the specified index
template <std::size_t I, class T>
struct pack_element;

// Returns the element of the pack type at the specified index
template <std::size_t I, class T>
using pack_element_t = typename pack_element<I, T>::type;
// -------------------------------------------------------------------------- //
// The interface to rebind a pack type to another set of template parameters
template <class T, class... Args>
struct pack_rebind;

// Rebinds a pack type to another set of template parameters
template <class T, class... Args>
using pack_rebind_t = typename pack_rebind<T, Args...>::type;
// ========================================================================== //



// =========================== PACK TYPE CONCEPT ============================ //
// The base class to check if a type is a pack: the type is not a pack
template <class T, class = void>
struct is_pack: std::false_type {};

// Checks if a type is a pack through pack_size, pack_element, and pack_rebind
template <class T>
struct is_pack<T, pack_element_t<
    pack_size_v<pack_rebind_t<T, void>> - 1, 
    pack_rebind_t<T, void>
>>: std::true_type {};

// Returns true when the provided type is a pack, and false otherwise
template <class T>
inline constexpr bool is_pack_v = is_pack<T>::value;
// -------------------------------------------------------------------------- //
// Conditionally produces a type if a pack type is provided: declaration
template <class T, class... Types>
struct if_pack;

// Conditionally produces a type if a pack type is provided: void only if true
template <class T>
struct if_pack<T>: std::enable_if<is_pack_v<T>> {};

// Conditionally produces a type if a pack type is provided: type only if true
template <class T, class True>
struct if_pack<T, True>: std::enable_if<is_pack_v<T>, True> {};

// Conditionally produces a type if a pack type is provided: conditional type
template <class T, class True, class False>
struct if_pack<T, True, False>: std::conditional<is_pack_v<T>, True, False> {};

// Conditionally produces a type if a pack type is provided
template <class T, class... Types>
using if_pack_t = typename if_pack<T, Types...>::type;
// ========================================================================== //



// =========================== INDEXING UTILITIES =========================== //
// Constant index type to index pack type elements
template <std::size_t I>
struct index: std::integral_constant<std::size_t, I> {};

// Makes an index type from I
template <std::size_t I>
using make_index = index<I>; 
// -------------------------------------------------------------------------- //
// An increasing sequence of indices to handle pack types implementations
template <std::size_t... I>
struct indexer: std::index_sequence<I...> {};

// Converts a standard index sequence to an indexer: declaration
template <class Seq>
struct to_indexer;

// Converts a standard index sequence to an indexer: definition
template <std::size_t... I>
struct to_indexer<std::index_sequence<I...>> {
    using type = indexer<I...>;
};

// Converts a standard index sequence to an indexer
template <class Seq>
using to_indexer_t = typename to_indexer<Seq>::type;

// Makes an indexer of N indices starting from 0
template <std::size_t N>
using make_indexer = to_indexer_t<std::make_index_sequence<N>>;

// Makes an indexer of sizeof...(Args) indices starting from 0
template <class... Args>
using indexer_for = to_indexer_t<std::index_sequence_for<Args...>>;

// Makes an indexer using the size of a pack type
template <class Pack>
using pack_indexer = make_indexer<pack_size_v<Pack>>;
// ========================================================================== //



// ========================== PACK TYPES INSTANCES ========================== //
// An indexed element of a pack
template <class I, class T>
struct pack_item {
    using index = I;
    using type = T;
    constexpr pack_item operator[](index) const noexcept {};
};

// The base class of a template parameter pack wrapper: declaration
template <class Idx, class... Args>
struct pack_base;

// The base class of a template parameter pack wrapper: definition
template <std::size_t... I, class... Args>
struct pack_base<indexer<I...>, Args...>: pack_item<make_index<I>, Args>... {
    using pack_item<make_index<I>, Args>::operator[]...;
};

// A wrapper for template parameter packs
template <class... Args>
struct pack: pack_base<indexer_for<Args...>, Args...> {};
// -------------------------------------------------------------------------- //
// Returns the size of a pack
template <class... Args>
struct pack_size<pack<Args...>>
: std::integral_constant<std::size_t, sizeof...(Args)> {};

// Returns the element of the pack at the specified index
template <std::size_t I, class... Args>
struct pack_element<I, pack<Args...>> {
    using type = typename decltype(pack<Args...>{}[make_index<I>{}])::type;
};

// Rebinds a pack to another set of template parameters
template <class... Types, class... Args>
struct pack_rebind<pack<Types...>, Args...> {
    using type = pack<Args...>;
};
// -------------------------------------------------------------------------- //
// Returns the size of a standard tuple
template <class... Args>
struct pack_size<std::tuple<Args...>>
: std::integral_constant<std::size_t, sizeof...(Args)> {};

// Returns the element of the standard tuple at the specified index
template <std::size_t I, class... Args>
struct pack_element<I, std::tuple<Args...>> {
    using type = std::tuple_element_t<I, std::tuple<Args...>>;
};

// Rebinds a standard tuple to another set of template parameters
template <class... Types, class... Args>
struct pack_rebind<std::tuple<Types...>, Args...> {
    using type = std::tuple<Args...>;
};
// ========================================================================== //



// ======================== PACK TYPE ELEMENT ACCESS ======================== //
// Returns the pack type's first element recursively at level L: declaration
template <class Pack, std::size_t L = 1, class = void>
struct pack_front;

// Returns the pack type's first element recursively at level L: recursion
template <class Pack, std::size_t L>
struct pack_front<Pack, L, std::enable_if_t<(pack_size_v<Pack> > 0 && L > 1)>> {
    using type = typename pack_front<pack_element_t<0, Pack>, L - 1>::type;
};

// Returns the pack type's first element recursively at level L: termination
template <class Pack>
struct pack_front<Pack, 1, std::enable_if_t<(pack_size_v<Pack> > 0)>> {
    using type = pack_element_t<0, Pack>;
};

// Returns the pack type's first element recursively at level L
template <class Pack, std::size_t L = 1>
using pack_front_t = typename pack_front<Pack, L>::type;
// -------------------------------------------------------------------------- //
// Returns the pack type's last element recursively at level L: declaration
template <class Pack, std::size_t L = 1, class = void>
struct pack_back;

// Returns the pack type's last element recursively at level L: recursion
template <class Pack, std::size_t L>
struct pack_back<Pack, L, std::enable_if_t<(pack_size_v<Pack> > 0 && L > 1)>> {
    using type = typename pack_back<
        pack_element_t<pack_size_v<Pack> - 1, Pack>, L - 1
    >::type;
};

// Returns the pack type's last element recursively at level L: termination
template <class Pack>
struct pack_back<Pack, 1, std::enable_if_t<(pack_size_v<Pack> > 0)>> {
    using type = pack_element_t<pack_size_v<Pack> - 1, Pack>;
};

// Returns the pack type's last element recursively at level L
template <class Pack, std::size_t L = 1>
using pack_back_t = typename pack_back<Pack, L>::type;
// -------------------------------------------------------------------------- //
// Returns the pack type element if valid, another type otherwise: declaration
template <std::size_t I, class Pack, class T = void, class = void>
struct pack_element_or;

// Returns the pack type element if valid, another otherwise: valid
template <std::size_t I, class Pack, class T>
struct pack_element_or<I, Pack, T, std::enable_if_t<(I < pack_size_v<Pack>)>> {
    using type = pack_element_t<I, Pack>;
};

// Returns the pack type element if valid, another type otherwise: invalid
template <std::size_t I, class Pack, class T>
struct pack_element_or<I, Pack, T, std::enable_if_t<(I >= pack_size_v<Pack>)>> {
    using type = T;
};

// Returns the pack type element if valid, another otherwise
template <std::size_t I, class Pack, class T = void>
using  pack_element_or_t = typename pack_element_or<I, Pack, T>::type;
// -------------------------------------------------------------------------- //
// Gets an instantiation of the element of the pack type at the specified index
template <std::size_t I, class Pack, class = if_pack_t<Pack>>
constexpr pack_element<I, Pack> pack_get(Pack) {
    return {};
}

// Gets an instantiation of the element or type of the pack type
template <std::size_t I, class T, class Pack, class = if_pack_t<Pack>>
constexpr pack_element_or<I, Pack, T> pack_get_or(Pack) {
    return {};
}
// ========================================================================== //



// ========================== PACK TYPE OPERATIONS ========================== //
// Truncates a pack type by keeping only the first elements: declaration
template <class Pack, std::size_t N, class = make_indexer<N>>
struct pack_truncate;

// Truncates a pack type by keeping only the first elements: definition
template <class Pack, std::size_t N, std::size_t... I>
struct pack_truncate<Pack, N, indexer<I...>> {
    using type = pack_rebind_t<Pack, pack_element_t<I, Pack>...>;
};

// Truncates a pack type by keeping only the first elements
template <class Pack, std::size_t N>
using pack_truncate_t = typename pack_truncate<Pack, N>::type;
// -------------------------------------------------------------------------- //
// Resizes a pack type with added elements at the end if necessary: declaration
template <class Pack, std::size_t N, class T = void, class = make_indexer<N>>
struct pack_resize;

// Resizes a pack type with added elements at the end if necessary: definition
template <class Pack, std::size_t N, class T, std::size_t... I>
struct pack_resize<Pack, N, T, indexer<I...>> {
    using type = pack_rebind_t<Pack, pack_element_or_t<I, Pack, T>...>;
};

// Resizes a pack type with added elements at the end if necessary
template <class Pack, std::size_t N, class T = void>
using pack_resize_t = typename pack_resize<Pack, N, T>::type;
// -------------------------------------------------------------------------- //
// Appends an element at the end of the pack type: declaration
template <class Pack, class Arg, class = pack_indexer<Pack>>
struct pack_append;

// Appends an element at the end of the pack type: definition
template <class Pack, class Arg, std::size_t... I>
struct pack_append<Pack, Arg, indexer<I...>> {
    using type = pack_rebind_t<Pack, pack_element_t<I, Pack>..., Arg>;
};

// Appends an element at the end of the pack type
template <class Pack, class Arg>
using pack_append_t = typename pack_append<Pack, Arg>::type;
// -------------------------------------------------------------------------- //
// Swaps two elements of a pack at the provided indices: declaration
template <class Pack, std::size_t I, std::size_t J, class = pack_indexer<Pack>>
struct pack_swap;

// Swaps two elements of a pack at the provided indices: definition
template <class Pack, std::size_t I, std::size_t J, std::size_t... K>
struct pack_swap<Pack, I, J, indexer<K...>> {
    using type = pack_rebind_t<Pack, 
        pack_element_t<K != I && K != J ? K : K == I ? J : I, Pack>...
    >;
};

// Swaps two elements of a pack at the provided indices:
template <class Pack, std::size_t I, std::size_t J>
using pack_swap_t = typename pack_swap<Pack, I, J>::type;
// ========================================================================== //



// ========================= METAFUNCTION UTILITIES ========================= //
// An unwrapper to recursively remove layers of wrappers: termination
template <class T>
struct unwrapper {
    using type = T;
};

// Removes wrappers recursively to access the underlying type
template <class T>
using unwrap_t = typename unwrapper<T>::type;
// -------------------------------------------------------------------------- //
// A wrapper for types
template <class T>
struct type_wrapper {
    using type = T;
};

// Makes a type wrapper to wrap a type
template <class T>
using wrap_type = type_wrapper<T>;

// An unwrapper to recursively remove layers of wrappers: type wrapper removal
template <class T>
struct unwrapper<type_wrapper<T>>: unwrapper<T> {};
// -------------------------------------------------------------------------- //
// The base class to wrap a trait with parameters: invalid application
template <template <class...> class Trait, class = void, class... Types>
struct trait_wrapper_base {};

// The base class to wrap a trait with parameters: valid application
template <template <class...> class Trait, class... Types>
struct trait_wrapper_base<Trait, std::void_t<
    typename Trait<Types...>::type
>, Types...> {
    using type = typename Trait<Types...>::type;
};

// A wrapper for traits with parameters
template <template <class...> class Trait, class... Types>
struct trait_wrapper: trait_wrapper_base<Trait, void, Types...> {
    template <class... Args>
    using trait = Trait<Types..., Args...>;
    template <class... Args>
    using trait_t = typename Trait<Types..., Args...>::type;
};

// Makes a trait wrapper to wrap a trait with parameters
template <template <class...> class Trait, class... Types>
using wrap_trait = trait_wrapper<Trait, Types...>;

// An unwrapper to recursively remove layers of wrappers: trait wrapper removal
template <template <class...> class Trait, class... Args>
struct unwrapper<trait_wrapper<Trait, Args...>>: unwrapper<
    typename trait_wrapper<Trait, Args...>::type
> {};
// -------------------------------------------------------------------------- //
// The base class to select the form of metafunction invocation: invalid form
template <class F, class = void, class... Args>
struct invoke_metafunction_base {};

// The base class to select the form of metafunction invocation: trait
template <template <class...> class Trait, class... Args>
struct invoke_metafunction_base<trait_wrapper<Trait>, std::void_t<
    typename Trait<Args...>::type
>, Args...> {
    using type = unwrap_t<typename Trait<Args...>::type>;
};

// The base class to select the form of metafunction invocation: templated call
template <class Class, class Arg, class... Args>
struct invoke_metafunction_base<Class, std::void_t<
    decltype(std::declval<Class>().template operator()<Arg, Args...>())
>, Arg, Args...> {
    using type = unwrap_t<decltype(
        std::declval<Class>().template operator()<Arg, Args...>()
    )>;
};

// The base class to select the form of metafunction invocation: invoke result
template <class F, class... Args>
struct invoke_metafunction_base<F, std::void_t<
    std::invoke_result_t<F, Args...>
>, Args...> {
    using type = unwrap_t<std::invoke_result_t<F, Args...>>;
};

// Invokes a metafunction with the provided arguments: base class delegation
template <class F, class... Args>
struct invoke_metafunction: invoke_metafunction_base<F, void, Args...> {};

// Invokes a metafunction with the provided arguments
template <class F, class... Args>
using invoke_metafunction_t = typename invoke_metafunction<F, Args...>::type;
// -------------------------------------------------------------------------- //
// A type identity metafunction
template <class T>
struct type_identity {
    using type = T;
};

// A wrapped identity trait to serve as a reference metafunction
using identity_metafunction = wrap_trait<type_identity>;
// ========================================================================== //



// ==================== COMPUTATION OF PACK PERMUTATIONS ==================== //
// Generates all permutations of elements of a pack type: recursion
template <
    class Pack, 
    std::size_t N = pack_size_v<Pack>, 
    std::size_t I = 0, 
    class Packs = pack_rebind_t<Pack>
> struct generate_permutations {
    using type = typename generate_permutations<
        pack_swap_t<
            pack_back_t<Packs, N - 1>, N % 2 == 0 ? I : 0, N - 1
        >, N, I + 1,
        pack_append_t<Packs, typename generate_permutations<Pack, N - 1>::type>
    >::type;
};

// Generates all permutations of elements of a pack type: loop beginning
template <class Pack, std::size_t N, class Packs>
struct generate_permutations<Pack, N, 0, Packs> {
    using type = typename generate_permutations<
        pack_swap_t<Pack, 0, N - 1>, N, 1,
        pack_append_t<Packs, typename generate_permutations<Pack, N - 1>::type>
    >::type;
};

// Generates all permutations of elements of a pack type: loop end
template <class Pack, std::size_t N, class Packs>
struct generate_permutations<Pack, N, N, Packs> {
    using type = Packs;
};

// Generates all permutations of elements of a pack type: termination
template <class Pack, class Packs>
struct generate_permutations<Pack, 1, 0, Packs> {
    using type = Pack;
};

// Generates all permutations of elements of a pack type: empty pack type
template <class Pack, class Packs>
struct generate_permutations<Pack, 0, 0, Packs> {
    using type = Pack;
};

// Generates all permutations of elements of a pack type
template <class Pack>
using generate_permutations_t = typename generate_permutations<Pack>::type;
// -------------------------------------------------------------------------- //
// Transforms permutations by applying a metafunction to packs: declaration
template <
    class Perms, 
    class F = identity_metafunction, 
    class = pack_indexer<Perms>, 
    class = void
> struct transform_permutations;

// Transforms permutations by applying a metafunction to packs: recursion
template <class Perms, class F, std::size_t... I> 
struct transform_permutations<Perms, F, indexer<I...>, void> {
    using type = pack_rebind_t<Perms, typename transform_permutations<
        pack_element_t<I, Perms>, F, make_indexer<sizeof...(I) - 1>
    >::type...>;
};

// Transforms permutations by applying a metafunction to packs: binary function
template <class Perms, class F>
struct transform_permutations<Perms, F, indexer<0, 1>, std::void_t<
    invoke_metafunction_t<F, pack_element_t<0, Perms>, pack_element_t<1, Perms>>
>> {
    using type = pack_rebind_t<Perms, invoke_metafunction_t<
        F, pack_element_t<0, Perms>, pack_element_t<1, Perms>
    >>;
};

// Transforms permutations by applying a metafunction to packs: unary function
template <class Perms, class F>
struct transform_permutations<Perms, F, indexer<0>, std::void_t<
    invoke_metafunction_t<F, Perms>
>> {
    using type = invoke_metafunction_t<F, Perms>;
};

// Transforms permutations by applying a metafunction to packs
template <class Perms, class F = identity_metafunction>
using transform_permutations_t 
= typename transform_permutations<Perms, F>::type;
// -------------------------------------------------------------------------- //
// Generates and transforms all pack type permutations: definition
template <class Pack, class F = identity_metafunction>
struct compute_permutations {
    using type = transform_permutations_t<generate_permutations_t<Pack>, F>;
};

// Generates and transforms all pack type permutations
template <class Pack, class F = identity_metafunction>
using compute_permutations_t = typename compute_permutations<Pack, F>::type;
// ========================================================================== //



// ========================== GENERIC BENCHMARKING ========================== //
// Computes and hashes permutations from pack and metafunction: pack, function 
template <
    class Pack, 
    class F = identity_metafunction, 
    std::size_t N = pack_size_v<if_pack_t<Pack, Pack, F>>,
    class = void
> struct computation {
    using pack = pack_truncate_t<Pack, N>;
    using metafunction = F;
    static constexpr std::size_t size = pack_size_v<pack>;
    struct result {
        using type = compute_permutations_t<pack, metafunction>;
    };
    constexpr std::size_t operator()() const noexcept {
        return typeid(typename result::type).hash_code();
    }
};

// Computes and hashes permutations from pack and metafunction: function, pack  
template <class F, class Pack, std::size_t N>
struct computation<F, Pack, N, if_pack_t<Pack>> {
    using pack = pack_truncate_t<Pack, N>;
    using metafunction = F;
    static constexpr std::size_t size = pack_size_v<pack>;
    struct result {
        using type = compute_permutations_t<pack, metafunction>;
    };
    constexpr std::size_t operator()() const noexcept {
        return typeid(typename result::type).hash_code();
    }
};

// Returns the result type of the computation of permutations
template <
    class FPack1, 
    class FPack2 = identity_metafunction, 
    std::size_t N = pack_size_v<if_pack_t<FPack1, FPack1, FPack2>>
> using computation_t = typename computation<FPack1, FPack2, N>::result::type;
// -------------------------------------------------------------------------- //
// Universal interface to compute and hash permutations: first, second
template <
    class FPack1, 
    class FPack2 = identity_metafunction,
    class = std::enable_if_t<is_pack_v<FPack1> != is_pack_v<FPack2>>,
    std::size_t N = pack_size_v<if_pack_t<FPack1, FPack1, FPack2>>
> constexpr std::size_t compute(FPack1 = {}, FPack2 = {}) noexcept {
    volatile std::size_t value = computation<FPack1, FPack2, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: first, second, size
template <
    class FPack1, 
    class FPack2,
    std::size_t N
> constexpr std::size_t compute(FPack1 = {}, FPack2 = {}) noexcept {
    volatile std::size_t value = computation<FPack1, FPack2, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: second, first, size
template <
    class FPack2, 
    class FPack1,
    std::size_t N = pack_size_v<if_pack_t<FPack2, FPack2, FPack1>>,
    class... Dummy, 
    class = std::enable_if_t<sizeof...(Dummy) == 0>
> constexpr std::size_t compute(FPack1, FPack2 = {}, Dummy...) noexcept {
    volatile std::size_t value = computation<FPack2, FPack1, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: first, size, second
template <
    class FPack1,
    std::size_t N,
    class FPack2 = identity_metafunction
> constexpr std::size_t compute(FPack1 = {}, FPack2 = {}) noexcept {
    volatile std::size_t value = computation<FPack1, FPack2, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: second, size, first
template <
    class FPack2, 
    std::size_t N,
    class FPack1,
    class... Dummy, 
    class = std::enable_if_t<sizeof...(Dummy) == 0>
> constexpr std::size_t compute(FPack1, FPack2 = {}, Dummy...) noexcept {
    volatile std::size_t value = computation<FPack2, FPack1, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: size, first, second
template <
    std::size_t N,
    class FPack1,
    class FPack2 = identity_metafunction
> constexpr std::size_t compute(FPack1 = {}, FPack2 = {}) noexcept {
    volatile std::size_t value = computation<FPack1, FPack2, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: size, second, first
template <
    std::size_t N,
    class FPack2, 
    class FPack1,
    class... Dummy, 
    class = std::enable_if_t<sizeof...(Dummy) == 0>
> constexpr std::size_t compute(FPack1, FPack2 = {}, Dummy...) noexcept {
    volatile std::size_t value = computation<FPack2, FPack1, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: pack, trait, size
template <
    class Pack, 
    template <class...> class Trait,
    std::size_t N = pack_size_v<if_pack_t<Pack, Pack, wrap_trait<Trait>>>,
    class... Dummy, 
    class = std::enable_if_t<sizeof...(Dummy) == 0>,
    class F = wrap_trait<Trait>
> constexpr std::size_t compute(Pack = {}, F = {}) noexcept {
    volatile std::size_t value = computation<Pack, F, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: trait, pack, size
template <
    template <class...> class Trait, 
    class Pack,
    std::size_t N = pack_size_v<if_pack_t<Pack, Pack, wrap_trait<Trait>>>,
    class... Dummy, 
    class = std::enable_if_t<sizeof...(Dummy) == 0>,
    class F = wrap_trait<Trait>
> constexpr std::size_t compute(Pack = {}, F = {}) noexcept {
    volatile std::size_t value = computation<F, Pack, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: pack, size, trait
template <
    class Pack, 
    std::size_t N,
    template <class...> class Trait,
    class... Dummy, 
    class = std::enable_if_t<sizeof...(Dummy) == 0>,
    class F = wrap_trait<Trait>
> constexpr std::size_t compute(Pack = {}, F = {}) noexcept {
    volatile std::size_t value = computation<Pack, F, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: trait, size, pack
template <
    template <class...> class Trait,
    std::size_t N,
    class Pack,
    class... Dummy, 
    class = std::enable_if_t<sizeof...(Dummy) == 0>,
    class F = wrap_trait<Trait>
> constexpr std::size_t compute(Pack = {}, F = {}) noexcept {
    volatile std::size_t value = computation<F, Pack, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: size, pack, trait
template <
    std::size_t N,
    class Pack, 
    template <class...> class Trait,
    class... Dummy, 
    class = std::enable_if_t<sizeof...(Dummy) == 0>,
    class F = wrap_trait<Trait>
> constexpr std::size_t compute(Pack = {}, F = {}) noexcept {
    volatile std::size_t value = computation<Pack, F, N>{}();
    return value;
}

// Universal interface to compute and hash permutations: size, trait, pack
template <
    std::size_t N,
    template <class...> class Trait,
    class Pack,
    class... Dummy, 
    class = std::enable_if_t<sizeof...(Dummy) == 0>,
    class F = wrap_trait<Trait>
> constexpr std::size_t compute(Pack = {}, F = {}) noexcept {
    volatile std::size_t value = computation<F, Pack, N>{}();
    return value;
}
// ========================================================================== //



// ========================================================================== //
} // namespace metabench
#endif // _METABENCH_HPP_INCLUDED
// ========================================================================== //
