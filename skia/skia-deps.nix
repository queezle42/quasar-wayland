{ fetchgit }:
{
  angle2 = fetchgit {
    url = "https://chromium.googlesource.com/angle/angle.git";
    rev = "e04b7c7392d9d05907db6d453c8b8e577d306a7e";
    sha256 = "134313bxjh40lck4x5k148saphqp7hyq1wsal88npv10hnsqc1ny";
  };
  brotli = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/google/brotli.git";
    rev = "6d03dfbedda1615c4cba1211f8d81735575209c8";
    sha256 = "0l1wk83y471766n0snqv4i7h9fvqj92rbi63n0y2wqfdix0w8wf2";
  };
  d3d12allocator = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/GPUOpen-LibrariesAndSDKs/D3D12MemoryAllocator.git";
    rev = "169895d529dfce00390a20e69c2f516066fe7a3b";
    sha256 = "1r7m8lf0nnzmpkciz5yd1b8l5g9rb75yqpvklh0y7lfvfbzx2dp4";
  };
  dawn = fetchgit {
    url = "https://dawn.googlesource.com/dawn.git";
    rev = "d3e0bd4770cc8115d1342a8dc051a36e50e8bd26";
    sha256 = "0iam5qgsqa1zwrjq6gynl63anwgvfc8s3jflnwzhkimqq79pdj7y";
  };
  jinja2 = fetchgit {
    url = "https://chromium.googlesource.com/chromium/src/third_party/jinja2";
    rev = "e2d024354e11cc6b041b0cff032d73f0c7e43a07";
    sha256 = "09z9pc24y1axjlqrk2gi1v0swli29xx7zyabdydqsg1zcck7yckq";
  };
  markupsafe = fetchgit {
    url = "https://chromium.googlesource.com/chromium/src/third_party/markupsafe";
    rev = "0bad08bb207bbfc1d6f3bbc82b9242b0c50e5794";
    sha256 = "1xy9qg3fa3z8z881j90bf5k2gphb89nk4345xzrmy13qn2j5shii";
  };
  abseil-cpp = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/abseil/abseil-cpp.git";
    rev = "334aca32051ef6ede2711487acf45d959e9bdffc";
    sha256 = "16maw1a25shjb99ysn12ca9mdjdwd9h8ac118ck52f5hhhxn4vdi";
  };
  dng_sdk = fetchgit {
    url = "https://android.googlesource.com/platform/external/dng_sdk.git";
    rev = "c8d0c9b1d16bfda56f15165d39e0ffa360a11123";
    sha256 = "1nlq082aij7q197i5646bi4vd2il7fww6sdwhqisv2cs842nyfwm";
  };
  egl-registry = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/KhronosGroup/EGL-Registry";
    rev = "b055c9b483e70ecd57b3cf7204db21f5a06f9ffe";
    sha256 = "11msmg8zww86mgk1imwvs4zr2c8jzghnlk2f2hvp1jlm2qk5gxgl";
  };
  emsdk = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/emscripten-core/emsdk.git";
    rev = "a896e3d066448b3530dbcaa48869fafefd738f57";
    sha256 = "02ffj9plb0ckn39bic77qsvv7wgg1vx67dzqss3h1rqhp5zbaa3s";
  };
  expat = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/libexpat/libexpat.git";
    rev = "441f98d02deafd9b090aea568282b28f66a50e36";
    sha256 = "14k4by1afkcn5r3zyjqqjllm3hdnm898bcznq8v3gpml08cc6x0m";
  };
  freetype = fetchgit {
    url = "https://chromium.googlesource.com/chromium/src/third_party/freetype2.git";
    rev = "47574f7ea445c8bb751da0fa716424c9c29a6807";
    sha256 = "064x9na83dvf44zd7a8ipff96j76zdxp6fdm063z4rhhig4f8lki";
  };
  harfbuzz = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/harfbuzz/harfbuzz.git";
    rev = "4cfc6d8e173e800df086d7be078da2e8c5cfca19";
    sha256 = "126r5ikhggalwmfrn0jrh7hx9jb32902jnajh0cvy4gv1k42vfxf";
  };
  highway = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/google/highway.git";
    rev = "424360251cdcfc314cfc528f53c872ecd63af0f0";
    sha256 = "0w5y8qwnvqq78pgc11bg4a4ilraymv57b95pljf1cqxwd17wkp1y";
  };
  icu = fetchgit {
    url = "https://chromium.googlesource.com/chromium/deps/icu.git";
    rev = "a0718d4f121727e30b8d52c7a189ebf5ab52421f";
    sha256 = "1qxws2p91f6dmhy7d3967r5ygz06r88pkmpm97px067x0zzdz384";
  };
  icu4x = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/unicode-org/icu4x.git";
    rev = "4f81635489681ecf7707623177123cb78d6a66a0";
    sha256 = "11ir5w718plaj0k3f7214bz7nyv3ncxyrx0c4akqa5cwfrnj5rdw";
  };
  imgui = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/ocornut/imgui.git";
    rev = "55d35d8387c15bf0cfd71861df67af8cfbda7456";
    sha256 = "1095zw0mvw24bnxya3fhiw4r393pzxvj99qimaj7hzz6rfb0c2qx";
  };
  libavif = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/AOMediaCodec/libavif.git";
    rev = "55aab4ac0607ab651055d354d64c4615cf3d8000";
    sha256 = "1pf4m9wxzgb5vqp3i9f5hyzldnvg3i3cmqx99w0aas7zyvyhi2zq";
  };
  libgav1 = fetchgit {
    url = "https://chromium.googlesource.com/codecs/libgav1.git";
    rev = "5cf722e659014ebaf2f573a6dd935116d36eadf1";
    sha256 = "00c8p7mc2irfi2l396n8ba35b0gvx9lcbj3h9xa84igchrlw1l93";
  };
  libgrapheme = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/FRIGN/libgrapheme/";
    rev = "c0cab63c5300fa12284194fbef57aa2ed62a94c0";
    sha256 = "0yfla31c2pzmv85a50x5jhbfq79pp8bzpnyg97pmiqmf7y3iiw1q";
  };
  libjpeg-turbo = fetchgit {
    url = "https://chromium.googlesource.com/chromium/deps/libjpeg_turbo.git";
    rev = "ed683925e4897a84b3bffc5c1414c85b97a129a3";
    sha256 = "0p0120dkqgb8b578kf0c3xcx717c6k3s8xhknp1frq2yk3g4z0hd";
  };
  libjxl = fetchgit {
    url = "https://chromium.googlesource.com/external/gitlab.com/wg1/jpeg-xl.git";
    rev = "a205468bc5d3a353fb15dae2398a101dff52f2d3";
    sha256 = "0qx1sfc30fv8ykvx99h9ag59x36jbzh770fma2fzrc397v615n79";
  };
  libpng = fetchgit {
    url = "https://skia.googlesource.com/third_party/libpng.git";
    rev = "144b348e072a78e8130ed0acc452c9f039a67bf2";
    sha256 = "0haayi2gcfwzff6b3qlxjc1s6yvhlq13va2xm572vqaip2mpncbr";
  };
  libwebp = fetchgit {
    url = "https://chromium.googlesource.com/webm/libwebp.git";
    rev = "2af26267cdfcb63a88e5c74a85927a12d6ca1d76";
    sha256 = "0xahv6vnc14n6nmmhj019331nga6nxl524l6wv3n36gvnp260m3c";
  };
  libyuv = fetchgit {
    url = "https://chromium.googlesource.com/libyuv/libyuv.git";
    rev = "d248929c059ff7629a85333699717d7a677d8d96";
    sha256 = "00056lkqywmp0y3ifqkcii1vm2siigdpinhw52hgnynp43zq9xf6";
  };
  microhttpd = fetchgit {
    url = "https://android.googlesource.com/platform/external/libmicrohttpd";
    rev = "748945ec6f1c67b7efc934ab0808e1d32f2fb98d";
    sha256 = "0y2wxkngr5y97lighzyilir3m2qzkas4kcrg7ivlw3f31z1zhxlk";
  };
  oboe = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/google/oboe.git";
    rev = "b02a12d1dd821118763debec6b83d00a8a0ee419";
    sha256 = "0k6kpgfi8zvghfihs2ysngk6zx2g7cbkjpvjfx94dkhf12gpbnpd";
  };
  opengl-registry = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/KhronosGroup/OpenGL-Registry";
    rev = "14b80ebeab022b2c78f84a573f01028c96075553";
    sha256 = "0zkyw99wkl1svibadm88sxl0iddqscfp64bnwaq8m4wj1yspfsk2";
  };
  perfetto = fetchgit {
    url = "https://android.googlesource.com/platform/external/perfetto";
    rev = "93885509be1c9240bc55fa515ceb34811e54a394";
    sha256 = "1rkcmzjgicx2742jgkx94c1nahhlr39d338nzfr3459skq86pxaf";
  };
  piex = fetchgit {
    url = "https://android.googlesource.com/platform/external/piex.git";
    rev = "bb217acdca1cc0c16b704669dd6f91a1b509c406";
    sha256 = "05ipmag6k55jmidbyvg5mkqm69zfw03gfkqhi9jnjlmlbg31y412";
  };
  sfntly = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/googlei18n/sfntly.git";
    rev = "b55ff303ea2f9e26702b514cf6a3196a2e3e2974";
    sha256 = "1qi5rfzmwfrji46x95g6dsb03i1v26700kifl2hpgm3pqhr7afpz";
  };
  swiftshader = fetchgit {
    url = "https://swiftshader.googlesource.com/SwiftShader";
    rev = "eb75201a4e0354a36d315dd01077092ec9aa2356";
    sha256 = "1nxyag0d6r48ydrjjsdl97za369snxicmmqxj0v2ha859fx8g9a8";
  };
  vulkanmemoryallocator = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator";
    rev = "a6bfc237255a6bac1513f7c1ebde6d8aed6b5191";
    sha256 = "1hpzjwl5bgqv9hmf1fdldihfllcbdg515f391a200klg0rnixdds";
  };
  vulkan-deps = fetchgit {
    url = "https://chromium.googlesource.com/vulkan-deps";
    rev = "13783d616289ab4ff6cad96e570d04183b24f2e0";
    sha256 = "0fl3a5f9ip10q13h24rbjkjl5w4r4xlxgr78c16vi2pi5dipxk5r";
  };
  spirv-cross = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/KhronosGroup/SPIRV-Cross";
    rev = "b8fcf307f1f347089e3c46eb4451d27f32ebc8d3";
    sha256 = "021x72k68z4q7idqf5ljhdnq8hlkz4vawfpspa5fq4nz6pscr38z";
  };
  spirv-headers = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/KhronosGroup/SPIRV-Headers.git";
    rev = "05cc486580771e4fa7ddc89f5c9ee1e97382689a";
    sha256 = "0z526x4qfv5qnf7376hripx9727jrb5lkdxlpw85rmax5ncq9hyf";
  };
  spirv-tools = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/KhronosGroup/SPIRV-Tools.git";
    rev = "dc6676445be97ab19d8191fee019af62e2aaf774";
    sha256 = "0nkv35siq7yj42cahyrnz09g3zcdaf65qpkcd80aa8lc3w6ib8kh";
  };
  vello = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/linebender/vello.git";
    rev = "e04b6028651dfd6b87067c0d27c1207c7f735a6d";
    sha256 = "09qjqmxgv96xmmixfp0d2nynvgxa8g3v9vypsmbnsqv1g5c28iag";
  };
  vulkan-headers = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-Headers";
    rev = "31aa7f634b052d87ede4664053e85f3f4d1d50d3";
    sha256 = "1aazv1d2dqj6hkb8szsp2z5jzjmlngcg20k6agvah82kmyy2zgsg";
  };
  vulkan-tools = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-Tools";
    rev = "069bd14c0548556e130fc9e205adc918b7a01891";
    sha256 = "01iqv7kqv6h8m521bw07g9aady5p96430150vgc544473y6v8j5k";
  };
  vulkan-utility-libraries = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-Utility-Libraries";
    rev = "1b8b60bf7f271a09eeda032d117d51a43ed506cd";
    sha256 = "0j2zdbf21h3adngykps60iachsqjf9vy4rn1cg59v2cdwj7j61xk";
  };
  unicodetools = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/unicode-org/unicodetools";
    rev = "66a3fa9dbdca3b67053a483d130564eabc5fe095";
    sha256 = "0545yg6wfjxkzfc4pxyd69vlbilk3p1ww12z2zq26xx3961392dz";
  };
  wuffs = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/google/wuffs-mirror-release-c.git";
    rev = "e3f919ccfe3ef542cfc983a82146070258fb57f8";
    sha256 = "0b04ksbkf4dcm9myvf1sx9aclyh8jbpkrgja3h1chkfjbzcdvgfz";
  };
  zlib = fetchgit {
    url = "https://chromium.googlesource.com/chromium/src/third_party/zlib";
    rev = "646b7f569718921d7d4b5b8e22572ff6c76f2596";
    sha256 = "0d3pv2vazr8haw4gcc8d2dyybx23nwaqfn5vfaxzdryrwi5gmn4c";
  };
}
