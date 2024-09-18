#include <iostream>
#include <map>
#include <unordered_map>
#include <vector>
#include <type_traits>

// Define a template structure for the iterator
template <typename Payload, typename OuterMap, typename InnerMap>
class MultiLevelIterator {
    using OuterIterator = typename OuterMap::const_iterator;
    using BoxIterator = typename std::vector<std::vector<std::vector<InnerMap>>>::const_iterator;
    using VectorIterator = typename std::vector<std::vector<InnerMap>>::const_iterator;
    using InnerVectorIterator = typename std::vector<InnerMap>::const_iterator;
    using InnerIterator = typename InnerMap::const_iterator;

    OuterIterator outer_it, outer_end;
    BoxIterator box_it, box_end;
    VectorIterator vec_it, vec_end;
    InnerVectorIterator inner_vec_it, inner_vec_end;
    InnerIterator inner_it, inner_end;

public:
    MultiLevelIterator(const OuterMap& outer_map)
        : outer_it(outer_map.begin()), outer_end(outer_map.end()) {
        if (outer_it != outer_end) {
            box_it = outer_it->second.begin();
            box_end = outer_it->second.end();
            if (box_it != box_end) {
                vec_it = box_it->begin();
                vec_end = box_it->end();
                if (vec_it != vec_end) {
                    inner_vec_it = vec_it->begin();
                    inner_vec_end = vec_it->end();
                    if (inner_vec_it != inner_vec_end) {
                        inner_it = inner_vec_it->begin();
                        inner_end = inner_vec_it->end();
                    }
                }
            }
        }
    }

    bool operator!=(const MultiLevelIterator& other) const {
        return outer_it != other.outer_it || box_it != other.box_it || vec_it != other.vec_it || inner_vec_it != other.inner_vec_it || inner_it != other.inner_it;
    }

    MultiLevelIterator& operator++() {
        if (inner_it != inner_end) {
            ++inner_it;
        }
        if (inner_it == inner_end) {
            ++inner_vec_it;
            if (inner_vec_it != inner_vec_end) {
                inner_it = inner_vec_it->begin();
                inner_end = inner_vec_it->end();
            } else {
                ++vec_it;
                if (vec_it != vec_end) {
                    inner_vec_it = vec_it->begin();
                    inner_vec_end = vec_it->end();
                    if (inner_vec_it != inner_vec_end) {
                        inner_it = inner_vec_it->begin();
                        inner_end = inner_vec_it->end();
                    }
                } else {
                    ++box_it;
                    if (box_it != box_end) {
                        vec_it = box_it->begin();
                        vec_end = box_it->end();
                        if (vec_it != vec_end) {
                            inner_vec_it = vec_it->begin();
                            inner_vec_end = vec_it->end();
                            if (inner_vec_it != inner_vec_end) {
                                inner_it = inner_vec_it->begin();
                                inner_end = inner_vec_it->end();
                            }
                        }
                    } else {
                        ++outer_it;
                        if (outer_it != outer_end) {
                            box_it = outer_it->second.begin();
                            box_end = outer_it->second.end();
                            if (box_it != box_end) {
                                vec_it = box_it->begin();
                                vec_end = box_it->end();
                                if (vec_it != vec_end) {
                                    inner_vec_it = vec_it->begin();
                                    inner_vec_end = vec_it->end();
                                    if (inner_vec_it != inner_vec_end) {
                                        inner_it = inner_vec_it->begin();
                                        inner_end = inner_vec_it->end();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return *this;
    }

    const Payload& operator*() const {
        return inner_it->second;
    }
};

template <typename Payload, size_t Size1, size_t Size2, size_t Size3, template <typename...> class OuterMapType = std::map, template <typename...> class InnerMapType = std::map>
class MultiLevelDataStructure {
public:
    using InnerMap = InnerMapType<size_t, Payload>;
    using Box = std::vector<std::vector<std::vector<InnerMap>>>;
    using DataMap = OuterMapType<size_t, Box>;

    MultiLevelDataStructure() {}

    // Method to get the size of the main map
    size_t size() const {
        return data.size();
    }

    // Overloaded size methods for partial sizes
    size_t size(size_t p) const {
        auto it = data.find(p);
        if (it != data.end()) {
            return it->second.size();
        }
        return 0;
    }

    size_t size(size_t p, int i) const {
        if (data.find(p) != data.end() && i < Size1) {
            return data.at(p)[i].size();
        }
        return 0;
    }

    size_t size(size_t p, int i, int j) const {
        if (data.find(p) != data.end() && i < Size1 && j < Size2) {
            return data.at(p)[i][j].size();
        }
        return 0;
    }

    size_t size(size_t p, int i, int j, int k) const {
        if (data.find(p) != data.end() && i < Size1 && j < Size2 && k < Size3) {
            return data.at(p)[i][j][k].size();
        }
        return 0;
    }

    // Method to check if an element exists
    bool exist(size_t p, int i, int j, int k, size_t l) const {
        if (data.find(p) != data.end() && i < Size1 && j < Size2 && k < Size3) {
            return data.at(p)[i][j][k].find(l) != data.at(p)[i][j][k].end();
        }
        return false;
    }

    // Method to access or create an element
    Payload* access_or_create(size_t p, int i, int j, int k, size_t l) {
        if (data.find(p) == data.end()) {
            data[p] = createBox();
        }
        return &(data[p][i][j][k][l]);
    }

    // Method to push an element
    bool push(size_t p, int i, int j, int k, size_t l, const Payload& payload, bool force = false) {
        if (exist(p, i, j, k, l) && !force) {
            return false;
        }
        *access_or_create(p, i, j, k, l) = payload;
        return true;
    }

    // Method to get a pointer to an element
    Payload* get(size_t p, int i, int j, int k, size_t l) {
        if (exist(p, i, j, k, l)) {
            return &(data[p][i][j][k][l]);
        }
        return nullptr;
    }

    // Method to pop an element
    Payload* pop(size_t p, int i, int j, int k, size_t l) {
        if (!exist(p, i, j, k, l)) {
            return nullptr;
        }
        Payload* payload = new Payload(data[p][i][j][k][l]);
        data[p][i][j][k].erase(l);
        return payload;
    }

    // Method to clear all elements
    void clear() {
        data.clear();
    }

    // Method to erase an element by key
    void erase(size_t p) {
        data.erase(p);
    }

    // Method to delete an element
    void erase(size_t p, int i, int j, int k, size_t l) {
        if (exist(p, i, j, k, l)) {
            data[p][i][j][k].erase(l);
        }
    }

    // Method to display the structure (for demonstration purposes)
    void display() const {
        for (const auto& [key, box] : data) {
            std::cout << "Key: " << key << std::endl;
            for (size_t i = 0; i < box.size(); ++i) {
                for (size_t j = 0; j < box[i].size(); ++j) {
                    for (size_t k = 0; k < box[i][j].size(); ++k) {
                        for (const auto& [innerKey, payload] : box[i][j][k]) {
                            std::cout << "  [" << i << "][" << j << "][" << k << "][" << innerKey << "] = " << payload << std::endl;
                        }
                    }
                }
            }
        }
    }

    // Iterator support
    using iterator = MultiLevelIterator<Payload, DataMap, InnerMap>;
    iterator begin() const { return iterator(data); }
    iterator end() const { return iterator(DataMap()); }

private:
    // Data structure to hold the multi-level data
    DataMap data;

    // Helper method to create a 3D vector with the templated sizes
    Box createBox() {
        return Box(Size1, std::vector<std::vector<InnerMap>>(Size2, std::vector<InnerMap>(Size3)));
    }
};
