{ fetchgit }:
{
  angle2 = fetchgit {
    url = "https://chromium.googlesource.com/angle/angle.git";
    rev = "66bc9cfa00143312cc7545556041622a92745a91";
    sha256 = "15fp5ddmiya2sy1bf0x633w7qxbzmah48jqlxg6d7y6f0rvrqzwq";
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
    rev = "1b2d3eb175fdf83e21e21b6167a34225a03d2bc2";
    sha256 = "0zx5xnh9f7xbg1cda6mq6k3y3w4fzlnl4mfxs8psf6hh77f4hb2r";
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
    rev = "a46424228f0998a72c715f32e18dca8a7a764c1f";
    sha256 = "0ikhqdsf4n2zi566d08hixvgds4d2wjds9l4fmzlmqn3v6krhmb4";
  };
  harfbuzz = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/harfbuzz/harfbuzz.git";
    rev = "b74a7ecc93e283d059df51ee4f46961a782bcdb8";
    sha256 = "11la2l2by0fwwbfzw5an0z46ajjlamvxx9pwwmj30bxgqnx2r5px";
  };
  highway = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/google/highway.git";
    rev = "424360251cdcfc314cfc528f53c872ecd63af0f0";
    sha256 = "0w5y8qwnvqq78pgc11bg4a4ilraymv57b95pljf1cqxwd17wkp1y";
  };
  icu = fetchgit {
    url = "https://chromium.googlesource.com/chromium/deps/icu.git";
    rev = "364118a1d9da24bb5b770ac3d762ac144d6da5a4";
    sha256 = "1y4y24xamafvghs4n2rjz477r7zsg5fgf15va122q5i2hg0jdfvy";
  };
  icu4x = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/unicode-org/icu4x.git";
    rev = "bcf4f7198d4dc5f3127e84a6ca657c88e7d07a13";
    sha256 = "1hfc031ihzk83ds3906m3x1fammz0bnc2m4n4m2k1hgkvsdvj5if";
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
    rev = "ed217e3e601d8e462f7fd1e04bed43ac42212429";
    sha256 = "0fv97xl79yhk35k076i9m067adl2ngpdmwmidy488slm7gqlr39j";
  };
  libwebp = fetchgit {
    url = "https://chromium.googlesource.com/webm/libwebp.git";
    rev = "845d5476a866141ba35ac133f856fa62f0b7445f";
    sha256 = "0fp9nxxddhxvikjn51r46sjrc3iiggr7mivabfgz02426skp0zll";
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
  swiftshader = fetchgit {
    url = "https://swiftshader.googlesource.com/SwiftShader";
    rev = "da334852e70510d259bfa8cbaa7c5412966b2f41";
    sha256 = "0l08scgk2pmhx6m6af5yc3jbmw5v38a2qvm9y9aj0g91rbhyx1ng";
  };
  vulkanmemoryallocator = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator";
    rev = "a6bfc237255a6bac1513f7c1ebde6d8aed6b5191";
    sha256 = "1hpzjwl5bgqv9hmf1fdldihfllcbdg515f391a200klg0rnixdds";
  };
  vulkan-deps = fetchgit {
    url = "https://chromium.googlesource.com/vulkan-deps";
    rev = "f1dcf238ad742f936794809f28b0ad0511b6585b";
    sha256 = "0hhfw3znisdpzcb2vyx66j5d3c4zx9387wvqjp0c83r28d93ykqi";
  };
  spirv-cross = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/KhronosGroup/SPIRV-Cross";
    rev = "b8fcf307f1f347089e3c46eb4451d27f32ebc8d3";
    sha256 = "021x72k68z4q7idqf5ljhdnq8hlkz4vawfpspa5fq4nz6pscr38z";
  };
  spirv-headers = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/KhronosGroup/SPIRV-Headers.git";
    rev = "49a1fceb9b1d087f3c25ad5ec077bb0e46231297";
    sha256 = "180dfnvfd5jc43j32ksy1zb0s7f33wvg8igjhdg1clwszb0c9jzs";
  };
  spirv-tools = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/KhronosGroup/SPIRV-Tools.git";
    rev = "199038f10cbe56bf7cbfeb5472eb0a25af2f09f5";
    sha256 = "18hnl7sjp49bk7s8hl0hxq3a6qrdw43vzzjl9z5lvh7diazb4wpq";
  };
  vello = fetchgit {
    url = "https://skia.googlesource.com/external/github.com/linebender/vello.git";
    rev = "6938a2893d6a2ba658709d1d04720f6c6033700f";
    sha256 = "0vcj4smvsp1ks4fms4xr5jwn95cvdx2w3hqx4fy0pyxpyq58kzw5";
  };
  vulkan-headers = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-Headers";
    rev = "5677bafb820e476441e9e1f745371b72133407d3";
    sha256 = "10jjwhp7jf5lmjxjg4klzkq1a83kzrxd4hfj982dqxgppnkhpckd";
  };
  vulkan-tools = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-Tools";
    rev = "df8e710224f563a04b7db2680f72d31619c4b259";
    sha256 = "04gm3a537vc596vnz972fzrpj2xiink14n298418a25b5ahv9yz8";
  };
  vulkan-utility-libraries = fetchgit {
    url = "https://chromium.googlesource.com/external/github.com/KhronosGroup/Vulkan-Utility-Libraries";
    rev = "358a107a6ff284906dcccbabe5b0183c03fd85b6";
    sha256 = "051h27j2nq7k5r630sd9sl0icwqc04l184sbd575vn7wh6j1h1j0";
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
