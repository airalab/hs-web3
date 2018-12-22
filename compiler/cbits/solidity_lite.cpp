#include <solidity_lite.h>
#include <libsolidity/interface/CompilerStack.h>
#include <liblangutil/SourceReferenceFormatter.h>

using namespace dev::solidity;
using namespace langutil;
using namespace std;

struct Solidity {
    Solidity() : compiler(new CompilerStack)
    {}

    ~Solidity()
    { delete compiler; }

    CompilerStack * compiler;
    map<string, dev::h160> libs;
};

#ifdef __cplusplus
extern "C"
{
#endif

namespace dev {
namespace solidity {
namespace lite {

void * create()
{
    return static_cast<void *>(new Solidity);
}

void destroy(void * self)
{
    delete static_cast<Solidity *>(self);
}

int addSource(void * self, const char * name, const char * source)
{
    return static_cast<Solidity*>(self)->compiler->addSource(name, source);
}

int addLibrary(void * self, const char * name, const char * address)
{
    static_cast<Solidity*>(self)->libs[name] = dev::h160(address);
    return 0;
}

int compile(void * self, int optimize)
{
    auto s = static_cast<Solidity *>(self);

    s->compiler->setOptimiserSettings(optimize);
    s->compiler->setLibraries(s->libs);
    s->compiler->reset(true);
    if (s->compiler->parseAndAnalyze()) 
        if (s->compiler->compile())
            return 0;

    return -1;
}

char * c_string(const std::string &str)
{
    auto c_str = (char *) calloc(1, str.size()+1);
    memcpy(c_str, str.c_str(), str.size());
    return c_str;
}

char * abi(void * self, const char * name)
{
    auto abi_value = static_cast<Solidity *>(self)->compiler->contractABI(name);
    return c_string(abi_value.toStyledString());
}

char * binary(void * self, const char * name)
{
    auto bin_object = static_cast<Solidity *>(self)->compiler->object(name);
    return c_string(bin_object.toHex());
}

char * errors(void * self)
{
    auto compiler = static_cast<Solidity *>(self)->compiler;
    stringstream error_stream;
	for (auto const& error: compiler->errors()) {
		SourceReferenceFormatter fmt(error_stream, [&](string const& _source) -> Scanner const& { return compiler->scanner(_source); });
        fmt.printExceptionInformation(*error, (error->type() == Error::Type::Warning) ? "Warning" : "Error");
    }
    return c_string(error_stream.str());
}

} // solidity
} // lite
} // dev

#ifdef __cplusplus
}
#endif
