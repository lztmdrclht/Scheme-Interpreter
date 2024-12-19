#ifndef SHARED_PTR
#define SHARED_PTR

#include <map>
#include <string>

template <typename T>
class SharedPtr {
public:
    SharedPtr() {
        _p = nullptr;
    };
    SharedPtr(T* p) {
        _p = p;
        if (_p != nullptr)
            number[_p]++;
    };
    ~SharedPtr() {
        if(_p != nullptr) {
            if (--number[_p] == 0)
                delete _p;
        }
        _p = nullptr;
    };
    SharedPtr(const SharedPtr & other) {
        _p = other._p;
        if (_p != nullptr)
            number[_p]++;
    };
    SharedPtr& operator= (const SharedPtr & other) {
        if (this != &other) {
            if(_p != nullptr) {
                if (--number[_p] == 0)
                    delete _p;
            }
            _p = other._p;
            if (_p != nullptr)
                number[_p]++;
        }
        return *this;
    }
    operator bool() const{
        return _p != nullptr;
    };
    size_t use_count() const {
        return number[_p];
    };
    T* get() const {
        return _p;
    };
    T& operator*() const {
        return *_p;
    };
    T* operator->() const {
        return _p;
    };
    void reset() {
        if(_p != nullptr) {
            if (--number[_p] == 0)
                delete _p;
        }
        _p = nullptr;
    };
    void reset(T* p) {
        if(_p != nullptr) {
            if (--number[_p] == 0)
                delete _p;
        }
        if (p != nullptr)
            number[p] ++;
        _p = p;
    };

private:
    // Add whatever you want to add.
    T* _p;
    static std::map<T*, int> number;
};

template <typename T>
std::map<T*, int> SharedPtr<T>::number = {
    {nullptr, 0}
};

template <typename T, typename... Args>
SharedPtr<T> make_shared(Args&&... args) {
    return SharedPtr<T>(new T(std::forward<Args>(args)...));
}

//Then implement the make_shared function
//I think I'd better leave you to design the prototype :)

//template <?>
//? make_shared(?);

#endif //SHARED_PTR