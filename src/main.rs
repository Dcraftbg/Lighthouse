#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]

use core::panic;
use std::{env::{self, current_dir}, thread, time, io::{Write, stderr}, ffi::OsStr, path::{PathBuf, Path}, process::{exit, Command, Stdio}, fs::{create_dir, File, OpenOptions, self}, rc::Rc, fmt::format, collections::HashMap};
use colorscape::*;
use colorscape::Color::Bold::*;
use colorscape::Color::RESET;
#[derive(Clone,Debug)]
struct ProgramLocation {
    file: Rc<String>,
    linenumber: usize,
    character:  usize,
}
impl ProgramLocation {
    fn loc_display(&self) -> String{
        format!("{}:{}:{}",self.file,self.linenumber,self.character)
    }
}
struct CfgLexer {
    src: Vec<char>,
    cursor: usize,
    loc: ProgramLocation
}
#[derive(PartialEq,Debug)]
enum IntrinsicType {
    OPENSQUARE,
    CLOSESQUARE,
    COMA,
}
impl IntrinsicType {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "["=>Some(Self::OPENSQUARE),
            "]"=>Some(Self::CLOSESQUARE),
            ","=>Some(Self::COMA),
            _ => None
        }
    }
    fn to_str(&self) -> &str {
        match self {
            Self::OPENSQUARE => "[",
            Self::CLOSESQUARE => "]",
            Self::COMA=>","
        }
    }
}
#[derive(PartialEq,Debug)]
enum SETOp {
    SET
}
impl SETOp {
    fn to_str(&self) -> &str {
        match self {
            Self::SET => "="
        }
    }
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "=" => Some(Self::SET),
            _ => None
        }
    }
}
#[derive(PartialEq,Debug)]
enum TokenType {
    WordType(String),
    StringType(String),
    NumberType(i64),
    IntrinsicType(IntrinsicType),
    SETOp(SETOp)
}
impl TokenType{
    fn to_string(&self) -> String {
        match self {
            Self::WordType(word) => format!("Word({})",word),
            Self::StringType(word) => format!("String(\"{}\")",word.escape_default()),
            Self::SETOp(setop) => format!("SETOp({})",setop.to_str()),
            Self::NumberType(n) => format!("Number({})",n),
            Self::IntrinsicType(i) => format!("Intrinsic({})",i.to_str()),
            _ => "None".to_owned()
        }
    }
}
#[derive(Debug)]
struct Token {
    loc: ProgramLocation,
    typ: TokenType
}
impl Token {
    fn loc_display(&self) -> String {
        self.loc.loc_display()
    }
}
#[derive(Debug,Clone)]
enum CfgVarType {
    STRING(String),
    NUMBER(i64),
    ARRAY(Vec<CfgVarType>)
}
impl CfgVarType {
    fn is_string(&self) -> bool {
        match self {
            Self::STRING(_) => true,
            _ => false
        }
    }
    fn is_array(&self) -> bool {
        match self {
            Self::ARRAY(_) => true,
            _ => false
        }
    }
    fn unwrap_array(&self) -> &Vec<CfgVarType> {
        match self {
            Self::ARRAY(v) => v,
            _ => panic!("Called unwarp_array on non Array!")
        }
    }
    fn unwrap_string(&self) -> &String {
        match self {
            Self::STRING(v) => v,
            _ => panic!("Called unwarp_string on non String!")
        }
    }
}
#[derive(Debug)]
struct CfgVar {
    typ: CfgVarType,
    loc: ProgramLocation,
}
impl CfgVar {
    fn loc_display(&self) -> String {
        self.loc.loc_display()
    }
}
#[derive(Debug)]
struct CfgBuild {
    vars: HashMap<String, CfgVar>
}
impl CfgBuild {
    fn new() -> Self {
        Self { vars: HashMap::new() }
    }
}
macro_rules! com_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(C) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
macro_rules! com_error {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(C) [ERROR] {}: {}", $location.loc_display(), message);
        exit(1);
    });
}
macro_rules! com_info {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(C) [INFO] {}: {}", $location.loc_display(), message);
    });
}
macro_rules! com_warn {
    ($location:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(C) [WARN] {}: {}", $location.loc_display(), message);
    });
}
macro_rules! com_assert {
    ($location:expr, $condition:expr, $($arg:tt)*) => ({
        if !($condition) {
            let message = format!($($arg)*);
            eprintln!("(C) [ERROR] {}: {}", $location.loc_display(), message);
            
            exit(1);
        }
    });
}
macro_rules! par_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(P) [ERROR] {}: {}", $token.loc_display(), message);
        exit(1);
    });
}
macro_rules! par_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(P) [ERROR] {}: {}", $token.loc_display(), message);
            exit(1);
        }
    });
}
macro_rules! par_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(P) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
macro_rules! par_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [INFO] {}: {}", $token.loc_display(), message);
    });
}
macro_rules! par_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(P) [WARN] {}: {}", $token.loc_display(), message);
    });
}
macro_rules! lex_error {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        eprintln!("(L) [ERROR] {}: {}", $token.loc_display(), message);
        exit(1);
    });
}
macro_rules! lex_assert {
    ($token:expr, $condition:expr, $($arg:tt)*) => ({
        if !$condition {
            let message = format!($($arg)*);
            eprintln!("(L) [ERROR] {}: {}", $token.loc_display(), message);
            exit(1);
        }
    });
}
macro_rules! lex_expect {
    ($location:expr, $expector:expr, $($arg:tt)*) => ({

        let message = format!($($arg)*);
        let message = format!("(L) [ERROR] {}: {}", $location.loc_display(), message);
        $expector.expect(&message)
    });
}
macro_rules! lex_info {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(L) [INFO] {}: {}", $token.loc_display(), message);
    });
}
macro_rules! lex_warn {
    ($token:expr, $($arg:tt)*) => ({
        let message = format!($($arg)*);
        println!("(L) [WARN] {}: {}", $token.loc_display(), message);
    });
}
fn unescape(stri: &String) -> String {
    let mut out = String::new();
    let mut chars = stri.chars().into_iter();
    while let Some(chr) = chars.next(){
        if chr == '/' {
            let nc = &chars.next();
            assert!(nc.is_some(),"Error: could not unescape string, invalid string escape symbol");
            let nc = nc.unwrap();
            match nc {
                'n' => {
                    out.push_str("\n");
                }
                'r' => {
                    out.push_str("\r");
                }
                't' => {
                    out.push_str("\t");
                }
                _ => {
                    out.push(nc);
                }
            }

        }
        else {
            out.push(chr)
        }
    }
    out
}
impl CfgLexer {
    fn is_newline(&mut self) -> bool {
        if let Some(c) = self.cchar_s() {
            c=='\n' || c=='\r'
        }
        else {
            true
        }

    }
    fn trim_left(&mut self) -> bool {
        if !self.is_not_empty(){
            return false;
        }
        while self.is_not_empty() && self.src.get(self.cursor).unwrap().is_whitespace() {
            if self.src.get(self.cursor).unwrap() == &'\n' {
                self.loc.linenumber += 1;
                self.loc.character = 0;
            }
            if self.cursor >= self.src.len(){
                break;
            }
            self.cursor += 1;
            self.loc.character+=1;
        }
        true
    }
    fn is_not_empty(&self) -> bool {
        self.cursor < self.src.len()
    }
    fn is_not_empty_offset(&self, offset: usize) -> bool {
        self.cursor+offset < self.src.len()
    }
    fn new(src: String) -> Self {
        Self {
            src: src.chars().collect(),
            cursor: 0,
            loc: ProgramLocation { file: Rc::new(String::new()), linenumber: 1, character: 0 }.into(),
        }
    }
    fn drop_char(&mut self) -> Option<char>{
        self.loc.character+=1;
        self.cursor+=1;
        self.cchar_s()
        //if self.is_not_empty() {
        //    let c = self.cchar_s();
        //    if let Some(c) = c {
        //        if c == '\n' {
        //            self.loc.linenumber += 1;
        //            self.loc.character = 0;
        //        } else {
        //            self.loc.character += 1;
        //        }
        //        self.cursor += 1;
        //    }
        //}
    }
    fn drop_line(&mut self) {
        while self.is_not_empty() && self.src.get(self.cursor).unwrap() != &'\n' {
            self.cursor += 1;
        }
        self.cursor += 1;
        self.loc.character = 0;
        self.loc.linenumber += 1;
    }
    fn loc_display(&self) -> String {
        self.loc.loc_display()
    }
    fn pop_symbol(&mut self) -> Option<String> {
        let mut o = String::new();
        let mut c = self.cchar_s()?;
        if !c.is_alphabetic() && c != '_' && c!='$'{
            return None;
        }
        while self.is_not_empty() && c.is_alphanumeric() || c=='_' || c=='$'{
            c = self.cchar_s()?;
            self.cursor += 1;
            o.push(c);
        }
        o.pop();
        self.cursor -= 1;
        self.loc.character += o.len();
        Some(o)
    }
    fn cchar(&mut self) -> char {
        self.src.get(self.cursor).unwrap().clone()
    }
    fn cchar_s(&mut self) -> Option<char> {
        self.src.get(self.cursor).clone().copied()
    }
    fn cchar_offset(&mut self, offset: usize) -> Option<char> {
        if self.is_not_empty_offset(offset) {
            return Some(self.src.get(self.cursor+offset).unwrap().clone())
        }
        None
    }



    fn next_line(&mut self) -> Option<Vec<Token>> {
        self.trim_left();
        let mut o: Vec<Token> = Vec::new();
        while let Some(t) = self.next(){
            o.push(t);
            if self.src[self.cursor]  == '\n' {
                break;
            }
        }
        Some(o)
    }
}
impl Iterator for CfgLexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.trim_left();
        let mut c = self.cchar_s()?;
        match c {
            '#' => {
                let nc = self.drop_char()?;
                if nc == '#' {
                    self.drop_line();
                    return self.next();
                }
                else {
                    lex_error!(self,"Error: Unexpected char after # {}",nc)
                }
            }
            '"' => {
                let mut shouldIgnoreNext: bool = false;
                self.cursor += 1;
                self.loc.character += 1;
                c = self.cchar_s()?;
                let mut outstr: String = String::new();
                while self.is_not_empty() && (shouldIgnoreNext || c != '\"'){
                    c = self.cchar_s()?;
                    if shouldIgnoreNext {
                        shouldIgnoreNext = false
                    }
                    if c == '/' {
                        shouldIgnoreNext = true
                    }
                    outstr.push(c);
                    self.cursor+=1;
                }
                outstr.pop();
                self.loc.character+=2+outstr.len();
                return Some(Token { loc: self.loc.clone(), typ: TokenType::StringType(outstr) });
            }
            '[' | ']' | ','=> {
                self.cursor += 1;
                self.loc.character += 1;                
                let tmp = c.to_string();
                return Some(Token { loc: self.loc.clone(), typ: TokenType::IntrinsicType(IntrinsicType::from_str(tmp.as_str()).expect("Unhandled :(")) });
            }
            _ => {
                if c.is_alphabetic() || c == '_' || c == '$'{
                    let outstr = self.pop_symbol()?;
                    return Some(Token { loc: self.loc.clone(), typ: TokenType::WordType(outstr) });
                }
                else if (c == '-' && self.cchar_offset(1)?.is_numeric()) || c.is_numeric(){
                    let mut outstr: String = String::new();
                    if c=='0' && self.cchar_offset(1).is_some() && self.cchar_offset(1).unwrap() == 'x' {
                        self.cursor += 2;
                        self.loc.character += 2;
                        while self.is_not_empty() && c.is_ascii_hexdigit() {
                            c = self.cchar_s()?;
                            if c == '_' {
                                self.cursor+=1;
                                c = self.cchar_s()?;
                                continue;
                            }
                            self.cursor += 1;
                            outstr.push(c);
                        }
                        outstr.pop();
                        self.cursor -= 1;
                        self.loc.character += outstr.len();
                        
                        if let Ok(val) = i64::from_str_radix(&outstr, 16) {
                            return Some(Token {loc:self.loc.clone(), typ: TokenType::NumberType(val)});
                        }
                        else {
                            panic!("Unknown number combo: {}",outstr);
                        }
                    }
                    
                    if c=='-' {
                        outstr.push(c);
                        self.cursor += 1;
                        self.loc.character += 1;
                        c = self.cchar_s()?;
                    }
                    while self.is_not_empty() && c.is_numeric() {
                        c = self.cchar_s()?;
                        if c == '_' {
                            self.cursor+=1;
                            c = self.cchar_s()?;
                            continue;
                        }
                        self.cursor += 1;
                        outstr.push(c);
                    }
                    outstr.pop();
                    self.cursor -= 1;
                    self.loc.character += outstr.len();
                    if let Ok(val) = outstr.parse::<i64>() {
                        return Some(Token {loc: self.loc.clone(), typ: TokenType::NumberType(val)});
                    }
                    else {
                        panic!("Unknown number combo: {}",outstr);
                    }
                }
                else {
                    let mut outstr: String = String::new();
                    while self.is_not_empty() && !c.is_alphabetic() && !c.is_numeric() && !c.is_whitespace() && c != ';' && c!=')' && c!='(' && c!='{' && c!='}' && c!='[' && c!=']' && c!='"'{
                        c = self.cchar_s()?;
                        self.cursor += 1;
                        outstr.push(c);
                    }
                    outstr.pop();
                    self.cursor -= 1;
                    self.loc.character += outstr.len();
                    if let Some(op) = SETOp::from_str(&outstr) {
                        return Some(Token { typ: TokenType::SETOp(op), loc: self.loc.clone() })
                    }
                    else {
                        return  Some(Token { typ: TokenType::WordType(outstr),  loc: self.loc.clone()  });
                    }
                }
            }
        }
        #[allow(unreachable_code)]
        None
    }
}

macro_rules! update_progress {
    ($($arg:tt)*) => ({
        let message = format!($($arg)*);
        let cover = " ".repeat(unsafe { LAST_MSG_LEN });
        print!("\r{}",cover);
        print!("\r{}",message);
        unsafe { LAST_MSG_LEN = message.len(); }

        std::io::stdout().flush().expect("Could not flush stdout");
    });
}
macro_rules! update_progress_expect {
    ( $expector:expr,$($arg:tt)*) => {{
        let message = format!($($arg)*);
        let cover = " ".repeat(unsafe { LAST_MSG_LEN });
        print!("\r{}",cover);
        let res = $expector.expect(&format!("\r{}",message));
        unsafe { LAST_MSG_LEN = message.len(); }
        std::io::stdout().flush().expect("Could not flush stdout");
        res
    }};
}

static mut LAST_MSG_LEN: usize = 0;


//lighthouse init
//lighthouse run
//lighthouse build
//lighthouse --arc="$REL: "
fn remove_arg_if(args: &mut Vec<String>) -> Option<String> {
    if args.len() > 0 {
        return Some(args.remove(0));
    }
    None
}
#[derive(Debug)]
struct PackageInfo {
    cfg: PathBuf,
    src_folder: PathBuf,
}
impl PackageInfo {
    fn new() -> Self{
        Self { cfg: PathBuf::new(), src_folder: PathBuf::new() }
    }
}
fn can_create_package(dir: String) -> bool {
    let mut pinfo = PackageInfo::new();
    pinfo.cfg = PathBuf::from(dir.clone()+"/lighthouse.cfg");
    pinfo.src_folder = PathBuf::from(dir.clone()+"/src");
    !pinfo.cfg.exists() && !pinfo.src_folder.exists()
}
fn verify_package_exists(dir: String) -> Result<PackageInfo, String> {
    let mut pinfo = PackageInfo::new();
    pinfo.cfg = PathBuf::from(dir.clone()+"/lighthouse.cfg");
    if !pinfo.cfg.exists() {
        return Err("cfg does not exist".to_owned());
    }
    if !pinfo.cfg.is_file()  {
        return Err("cfg was not a file".to_owned());
    }
    pinfo.src_folder = PathBuf::from(dir.clone()+"/src");
    if !pinfo.src_folder.exists() {
        return Err("src folder does not exist".to_owned());
    }
    if !pinfo.src_folder.is_dir()  {
        return Err("src was not a directory".to_owned());
    }
    Ok(pinfo)
}
fn parse_tokens_to_build(lexer: &mut CfgLexer) -> CfgBuild {
    #[allow(unused_mut)]
    let mut build = CfgBuild::new();
    while let Some(token) = lexer.next() {
        match token.typ {
            TokenType::WordType(word) => {
                par_assert!(lexer, !word.starts_with("$"),"Error: Cannot set Environmental Variable in config :(");
                let ntok = par_expect!(lexer,lexer.next(),"Error: Expected setop after word but found nothing");
                par_assert!(ntok,ntok.typ==TokenType::SETOp(SETOp::SET), "Error: Expected setop but found {}",ntok.typ.to_string());
                //println!("WHATDAFUCK");
                let ntok = par_expect!(lexer,lexer.next(),"Error: Expected value after setop but found nothing");
                //println!("\n\nntok: {:?}",ntok);
                match ntok.typ {
                    TokenType::WordType(v) => {
                        if v.starts_with("$") {
                            match env::var(&v[1..]) {
                                Ok(val) => {
                                    build.vars.insert(word, CfgVar { typ: CfgVarType::STRING(val), loc: ntok.loc });
                                }
                                Err(err) => {
                                    par_error!(lexer, "Error: Could not find environmental variable {}\nReason: {}",&v[1..],err.to_string())
                                }
                            }
                        }
                        else {
                            build.vars.insert(word, CfgVar { typ: par_expect!(lexer,build.vars.get(&v).as_ref(),"Error: expected set word value to be a variable but variable {} does not exit!",v).typ.clone(), loc: ntok.loc });
                        }
                    }
                    TokenType::NumberType(v) => {
                        build.vars.insert(word, CfgVar { typ: CfgVarType::NUMBER(v), loc: ntok.loc});
                    }
                    TokenType::StringType(v) => {
                        build.vars.insert(word, CfgVar { typ: CfgVarType::STRING(v), loc: ntok.loc});
                    }
                    TokenType::IntrinsicType(i) => {
                        par_assert!(lexer, i==IntrinsicType::OPENSQUARE, "Error: Unexpected intrinsic type in assignment {}",i.to_str());
                        let mut array_body: Vec<CfgVarType> = Vec::new();
                        while let Some(t) = lexer.next() {
                            match t.typ {
                                TokenType::WordType(v) => {
                                    if v.starts_with("$") {
                                        match env::var(&v[1..]) {
                                            Ok(val) => {
                                                array_body.push(CfgVarType::STRING(val))
                                            }
                                            Err(err) => {
                                                par_error!(lexer, "Error: Could not find environmental variable {}\nReason: {}",&v[1..],err.to_string())
                                            }
                                        }
                                    }
                                    else {
                                        array_body.push(par_expect!(lexer,build.vars.get(&v).as_ref(),"Error: expected element in array to be an exiting variable but variable {} does not exit!",v).typ.clone())
                                    }
                                }
                                TokenType::NumberType(v) => {
                                    array_body.push(CfgVarType::NUMBER(v))
                                }
                                TokenType::StringType(v) => {
                                    array_body.push(CfgVarType::STRING(v))
                                }
                                TokenType::IntrinsicType(i)  => {
                                    par_assert!(lexer, i==IntrinsicType::CLOSESQUARE, "Error: Unexpected intrinsic type {} in array definition ",i.to_str());
                                    break;
                                }
                                _ => par_error!(t,"Unexpected token type for element definition {}",t.typ.to_string())
                            }
                            let ntok = par_expect!(lexer,lexer.next(), "Error: Expected next Intrinsic to be , or ] but found nothing");
                            if ntok.typ == TokenType::IntrinsicType(IntrinsicType::CLOSESQUARE) {
                                break;
                            }
                            else if ntok.typ == TokenType::IntrinsicType(IntrinsicType::COMA)  {
                                continue;
                            }
                            else {
                                par_error!(ntok, "Error: Unexpected token type {} after element definition",ntok.typ.to_string())
                            }
                        }
                        build.vars.insert(word, CfgVar { typ: CfgVarType::ARRAY(array_body), loc: ntok.loc });
                    }
                    _ => {
                        
                        par_error!(ntok,"Error: Expected String or Number but found {}",ntok.typ.to_string())
                    }
                }
            }
            _ => todo!()
        }
    }
    build
}
fn build_package(cdir: PathBuf) -> (CfgBuild,CfgLexer) {
    update_progress!("   {}Verifying package{}",LIGHT_BLUE(),RESET());    
    let cdirstr = cdir.to_str().unwrap_or_default().to_owned();
    let res = verify_package_exists(cdirstr);
    if res.is_err() {
        let err = res.unwrap_err();
        update_progress!("   {}Error: Invalid package: {}{}",RED(),err,RESET());
        exit(1);
    }
    let res = res.unwrap();
    update_progress!("   {}Package verified{}",GREEN(),RESET());
    let f = fs::read_to_string(res.cfg);
    if f.is_err() {
        let err = f.unwrap_err();
        update_progress!("   {}Error: Could not open config: {}{}",RED(),err,RESET());
        exit(1);
    }
    let cfgstr = f.unwrap();
    let mut lexer = CfgLexer::new(cfgstr);
    update_progress!("    {}Parsing lighthouse.cfg{}",LIGHT_BLUE(),RESET());
    lexer.loc.file = Rc::new("lighthouse.cfg".to_owned());
    let build = parse_tokens_to_build(&mut lexer);
    update_progress!("   {}Parsed lighthouse.cfg successfully{}",GREEN(),RESET());
    {
        let arch = if let Some(arch) = build.vars.get("arch") {
            com_assert!(arch, arch.typ.is_string(), "Error: Expected arch to be string but found other");
            Some(arch.typ.unwrap_string())
        } else { None };
        let opmode = if let Some(opmode) = build.vars.get("mode") {
            com_assert!(opmode, opmode.typ.is_string(), "Error: Expected mode to be string but found other");
            Some(opmode.typ.unwrap_string().to_owned())
        } else { None };
        let mut oargs: Vec<String> = Vec::new();
        if let Some(otarget) = build.vars.get("target") {
            com_assert!(otarget.loc,otarget.typ.is_string(),"Error: Expected string but found other");
            oargs.extend(vec!["-t".to_owned(),otarget.typ.unwrap_string().clone()]);
        }
        
        let entry = build.vars.get("entry").expect("Error: Expected entry but found nothing! Cannot have build without entry specification for now (libs are yet to be implemented for lighthouse)");
        com_assert!(entry, entry.typ.is_string(), "Error: Expected value of entry to be string but found other");
        oargs.push(entry.typ.unwrap_string().clone());

        let entry_path = PathBuf::from(entry.typ.unwrap_string().clone());
        if !entry_path.exists() {
            update_progress!("   {}Error: entry path {} does not exist (entry defined {}){}\n",RED(),entry.typ.unwrap_string(),entry.loc_display(),RESET());
            exit(1);
        }
        if !entry_path.is_file() {
            update_progress!("   {}Error: entry path {} isn't a file (entry defined {}){}\n",RED(),entry.typ.unwrap_string(),entry.loc_display(),RESET());
            exit(1);
        }
        let int_rep = PathBuf::from(entry_path.file_stem().unwrap()).with_extension("asm");
        let int_rep_str = int_rep.to_str().unwrap_or_default();
        let intpath = build.vars.get("intpath").expect("Error: Expected entry but found nothing! Cannot have build without entry specification for now (libs are yet to be implemented for lighthouse)");
        com_assert!(intpath, intpath.typ.is_string(), "Error: Expected value of entry to be string but found other");
        if let Some(opmode) = &opmode {
            if opmode == "release" {
                oargs.extend(vec!["-release".to_owned()])
            }
        }
        oargs.extend(vec!["-o".to_owned(),intpath.typ.unwrap_string().clone()+"/"+int_rep_str]);
        if arch.is_some() {
            if arch.unwrap() == "custom" {
                let arch_path = com_expect!(lexer,build.vars.get("arch_path"),"Error: Expect arch_path with custom but found nothing!");
                com_assert!(arch_path,arch_path.typ.is_string(), "Errror: Expected string for arch_path but found something else!");
                oargs.extend(vec!["-arc".to_owned(),"-".to_owned(),arch_path.typ.unwrap_string().clone()])
            }
            else {
                oargs.extend(vec!["-arc".to_owned(),arch.unwrap().clone()])
            }
        }
        let get_var = "local_dependencies";
        let mut olinkpaths: Vec<String> = Vec::new();
        if let Some(obj_var) = build.vars.get(get_var) {
            com_assert!(obj_var,obj_var.typ.is_array(), "Error: expected {get_var} to be an array but found nothing!");
            let obj_var_ar = obj_var.typ.unwrap_array();
            olinkpaths = Vec::with_capacity(obj_var_ar.len());
            
            for obj in obj_var_ar {
                com_assert!(obj_var, obj.is_string(), "Error: Cannot have flags to {get_var} which are not strings");
                olinkpaths.push(obj.unwrap_string().clone())
            }
        }
        if olinkpaths.len() > 0 {
            oargs.push("-i".to_owned());
            let mut i: usize = 0;
            while i < olinkpaths.len() {
                if i == olinkpaths.len()-1 {
                    oargs.push(olinkpaths[i].clone())
                }
                else {
                    oargs.push(olinkpaths[i].clone()+",")
                }
                i+=1;
            }
        }

        update_progress!("   {}Running sopl {}{}",LIGHT_BLUE(),oargs.join(" "),RESET());
        let cmd = Command::new("sopl").args(oargs).stdout(Stdio::inherit()).stdin(Stdio::inherit()).stderr(Stdio::inherit()).spawn();
        
        if cmd.is_err() {
            println!();
            update_progress!("   {}Error: could not run sopl command{}\n",RED(),RESET());
            exit(1);
        }
        let mut cmd = cmd.unwrap();
        let status = cmd.wait();
        if status.is_err() {
            println!();
            update_progress!("   {}Error: could not recieve any information from sopl command{}\n",RED(),RESET());
            exit(1);
        }
        let status = status.unwrap();
        let ostatus = status.code().unwrap_or(1);
        if ostatus == 0 {
            update_progress!("   {}Finished{} built code successfully",GREEN(),RESET());
        }
        else {
            println!();
            update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}",RED(),RESET(),ostatus);
            exit(ostatus)
        }
        
        if let Some(build_files) = build.vars.get("build") {
            com_assert!(build_files,build_files.typ.is_array(), "Error: Expected build to be an array but found something else");
            let vals = build_files.typ.unwrap_array();
            let intpath = build.vars.get("intpath").expect("Error: Expected entry but found nothing! Cannot have build without entry specification for now (libs are yet to be implemented for lighthouse)");
            com_assert!(intpath, intpath.typ.is_string(), "Error: Expected value of entry to be string but found other");
            for val in vals.iter() {
                let mut oargs: Vec<String> = Vec::new();
                com_assert!(build_files, val.is_string(), "Error: All elements of build must be strings!");
                let val = val.unwrap_string();
                if let Some(opmode) = &opmode {
                    if opmode == "release" {
                        oargs.extend(vec!["-release".to_owned()])
                    }
                }
                oargs.push(val.clone());
                let v_ = PathBuf::from(val);
                let val = v_.file_stem().unwrap();
                let int_rep = PathBuf::from(val).with_extension("asm");
                let int_rep_str = int_rep.to_str().unwrap_or_default();
                oargs.extend(vec!["-o".to_owned(),intpath.typ.unwrap_string().clone()+"/"+int_rep_str]);
                if arch.is_some() {
                    if arch.unwrap() == "custom" {
                        let arch_path = com_expect!(lexer,build.vars.get("arch_path"),"Error: Expect arch_path with custom but found nothing!");
                        com_assert!(arch_path,arch_path.typ.is_string(), "Errror: Expected string for arch_path but found something else!");
                        oargs.extend(vec!["-arc".to_owned(),"-".to_owned(),arch_path.typ.unwrap_string().clone()])
                    }
                    else {
                        oargs.extend(vec!["-arc".to_owned(),arch.unwrap().clone()])
                    }
                }
                if olinkpaths.len() > 0 {
                    oargs.push("-i".to_owned());
                    let mut i: usize = 0;
                    while i < olinkpaths.len() {
                        if i == olinkpaths.len()-1 {
                            oargs.push(olinkpaths[i].clone())
                        }
                        else {
                            oargs.push(olinkpaths[i].clone()+",")
                        }
                        i+=1;
                    }
                }
                update_progress!("   {}Running sopl {}{}",LIGHT_BLUE(),oargs.join(" "),RESET());
                let cmd = Command::new("sopl").args(oargs).stdout(Stdio::inherit()).stdin(Stdio::inherit()).stderr(Stdio::inherit()).spawn();
                if cmd.is_err() {
                    update_progress!("   {}Error: could not run sopl command{}\n",RED(),RESET());
                    exit(1);
                }
                let mut cmd = cmd.unwrap();
                let status = cmd.wait();
                if status.is_err() {
                    update_progress!("   {}Error: could not recieve any information from sopl command{}\n",RED(),RESET());
                    exit(1);
                }
                let status = status.unwrap();
                let ostatus = status.code().unwrap_or(1);
                if ostatus == 0 {
                    update_progress!("   {}Finished{} built code successfully",GREEN(),RESET());
                }
                else {
                    update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}",RED(),RESET(),ostatus);
                    exit(ostatus)
                }
            }
        }
    }
    (build,lexer)
}
#[derive(Debug,Clone)]
struct ArcFlags {
    nasm: Vec<String>,
    gcc:  Vec<String>,
    ld:   Vec<String>
}
impl ArcFlags {
    fn new() -> Self {
        Self { nasm: Vec::new(), gcc: Vec::new(), ld: Vec::new() }
    }
}
#[derive(Debug,Clone)]
struct LHArchitecture {
    flags: ArcFlags,
    obj_extension: String,
    exe_extension: String
}
impl LHArchitecture {
    fn new() -> Self {
        Self { flags: ArcFlags::new(), obj_extension: String::new(), exe_extension: String::new() }
    }
}
enum PackageType {
    BIN,
    LIB
}
fn usage(program: String){
    println!("{} (command) [flags]",program);
    println!("   init                  // Initializes an new project");
    println!("      --type [bin|lib]   // Sets the type of project - library or program");
    println!("      --name [name]      // Sets the name of the project");
    println!("   build                 // Builds the assembly with sopl");
    println!("   run                   // runs the current project");
    println!("   verify                // verify everything is correct with the current project");
} 


fn main() {
    Color::init();
    let mut Architectures: HashMap<String, LHArchitecture> = HashMap::new();
    
    Architectures.insert("windows_x86_64".to_owned(), LHArchitecture{flags: ArcFlags { nasm: vec!["-f".to_string(),"win64".to_string()], gcc: vec!["-m64".to_string()], ld: Vec::new() }, obj_extension: "obj".to_owned(), exe_extension: "exe".to_owned()});
    Architectures.insert("windows_x86".to_owned(),    LHArchitecture{flags: ArcFlags { nasm: vec!["-f".to_string(),"win32".to_string()], gcc: vec!["-m32".to_string()], ld: Vec::new() }, obj_extension: "obj".to_owned(), exe_extension: "exe".to_owned()});
    Architectures.insert("linux_x86_64".to_owned(),   LHArchitecture{flags: ArcFlags { nasm: vec!["-f".to_string(),"elf64".to_string()], gcc: vec!["-m64".to_string()], ld: Vec::new() }, obj_extension: "o"  .to_owned(), exe_extension: "".to_owned()   });
    Architectures.insert("linux_x86".to_owned(),      LHArchitecture{flags: ArcFlags { nasm: vec!["-f".to_string(),"elf64".to_string()], gcc: vec!["-m32".to_string()], ld: Vec::new() }, obj_extension: "o"  .to_owned(), exe_extension: "".to_owned()   });
    //short calls
    Architectures.insert("win_x86_64".to_owned(), Architectures.get("windows_x86_64").unwrap().clone());
    Architectures.insert("win_x86".to_owned(), Architectures.get("windows_x86").unwrap().clone());
    let mut args: Vec<_> = env::args().collect();
    
    let program = remove_arg_if(&mut args).expect("Error: expected program but found nothing");
    let command = remove_arg_if(&mut args);
    if command.is_none() {
        usage(program);
        exit(1)
    }
    let command = command.unwrap();
    match command.as_str() {
        "help" => {
            usage(program);
            exit(0)
        }
        "verify" => {
            update_progress!("   {}Verifying package{}",LIGHT_BLUE(),RESET());
            let cdir = current_dir().unwrap_or_default();
            let cdirstr = cdir.to_str().unwrap_or_default().to_owned();
            let res = verify_package_exists(cdirstr);
            if res.is_err() {
                let err = res.unwrap_err();
                update_progress!("   {}Error: Invalid package: {}{}",RED(),err,RESET());
                exit(1);
            }
            update_progress!("   {}Package verified{}",GREEN(),RESET());
        }
        "build" => {
            build_package(current_dir().unwrap_or_default());
        }
        "run" => {
            let (build,lexer) = build_package(current_dir().unwrap_or_default());
            let ptyp = com_expect!(lexer, build.vars.get("type"), "Error: Expected type of package but found nothing!");
            com_assert!(ptyp, ptyp.typ.is_string(), "Error: Expected type of package to be string but found something else");
            let ptyp_str = ptyp.typ.unwrap_string();
            match ptyp_str.as_str() {
                "bin" => {}
                "lib" => {
                    update_progress!("   {}Failed{} Cannot run package of type lib! (type defined here: {})",RED(),RESET(),ptyp.loc_display());
                    exit(1);
                }
                _ => {
                    com_error!(ptyp, "Error: Unknown project typ: {}",ptyp_str);
                }
            }
            
            let target_typ = if let Some(otarget) = build.vars.get("target") {
                com_assert!(otarget.loc,otarget.typ.is_string(),"Error: Expected string but found other");
                otarget.typ.unwrap_string().clone()
            } else {
                "nasm_x86_64".to_owned()
            };
            
            match target_typ.as_str() {
                "nasm_x86_64" => {
                    let intpath = build.vars.get("intpath").expect("Error: Expected entry but found nothing! Cannot have build without entry specification for now (libs are yet to be implemented for lighthouse)");
                    com_assert!(intpath, intpath.typ.is_string(), "Error: Expected value of entry to be string but found other");
                    let intpath = intpath.typ.unwrap_string();
                    let binpath = build.vars.get("binpath").expect("Error: Expected entry but found nothing! Cannot have build without entry specification for now (libs are yet to be implemented for lighthouse)");
                    com_assert!(binpath, binpath.typ.is_string(), "Error: Expected value of entry to be string but found other");
                    let binpath = binpath.typ.unwrap_string();
                    let oarch = if let Some(architecture) = build.vars.get("arch") {
                        com_assert!(architecture,architecture.typ.is_string(), "Error: Expected string but found something else!");
                        architecture.typ.unwrap_string().to_owned()
                    } else {
                        "".to_owned()+env::consts::OS+"_"+env::consts::ARCH
                    };
                    let arc = if oarch == "custom" {
                        let arch_path = build.vars.get("arch_path").expect("Cannot have arch=\"custom\" without arch_path");
                        com_assert!(arch_path,arch_path.typ.is_string(), "Error: Expected string but found other!");
                        let arch_path_buf = PathBuf::from(arch_path.typ.unwrap_string()).with_extension("cfg");
                        com_assert!(arch_path,arch_path_buf.exists(), "Error: Expected \"{}\" to exist however it doesn't!",arch_path_buf.to_string_lossy());
                        let data = fs::read_to_string(arch_path_buf.clone());
                        if data.is_err() {
                            update_progress!("   {}Failed{} Could not read arch config for \"{}\"\nReason: {}",RED(),RESET(),arch_path_buf.to_string_lossy(),data.unwrap_err().to_string());
                            exit(1);
                        }
                        let data = data.unwrap();
                        let mut lexer = CfgLexer::new(data);
                        lexer.loc.file = Rc::new(arch_path_buf.to_str().unwrap().to_owned());
                        let build = parse_tokens_to_build(&mut lexer);
                        let mut outarch = LHArchitecture::new();
                        
                        let obj_var = com_expect!(lexer, build.vars.get("obj_extension"), "Error: Expected obj_extension for architecture build but found nothing!");
                        com_assert!(obj_var,obj_var.typ.is_string(), "Error: expected obj_extension to be string but found nothing!");
                        outarch.obj_extension = obj_var.typ.unwrap_string().clone();
                        let obj_var = com_expect!(lexer, build.vars.get("exe_extension"), "Error: Expected exe_extension for architecture build but found nothing!");
                        com_assert!(obj_var,obj_var.typ.is_string(), "Error: expected exe_extension to be string but found nothing!");
                        outarch.exe_extension = obj_var.typ.unwrap_string().clone();
        
        
        
                        let get_var = "flags_nasm";
                        let obj_var = com_expect!(lexer, build.vars.get(get_var), "Error: Expected {get_var} for architecture build but found nothing!");
                        com_assert!(obj_var,obj_var.typ.is_array(), "Error: expected {get_var} to be an array but found nothing!");
                        let obj_var_ar = obj_var.typ.unwrap_array();
                        let mut obody: Vec<String> = Vec::with_capacity(obj_var_ar.len());
                        for obj in obj_var_ar {
                            com_assert!(obj_var, obj.is_string(), "Error: Cannot have flags to {get_var} which are not strings");
                            obody.push(obj.unwrap_string().clone())
                        }
                        outarch.flags.nasm = obody;
        
                        let get_var = "flags_gcc";
                        let obj_var = com_expect!(lexer, build.vars.get(get_var), "Error: Expected {get_var} for architecture build but found nothing!");
                        com_assert!(obj_var,obj_var.typ.is_array(), "Error: expected {get_var} to be an array but found nothing!");
                        let obj_var_ar = obj_var.typ.unwrap_array();
                        let mut obody: Vec<String> = Vec::with_capacity(obj_var_ar.len());
                        for obj in obj_var_ar {
                            com_assert!(obj_var, obj.is_string(), "Error: Cannot have flags to {get_var} which are not strings");
                            obody.push(obj.unwrap_string().clone())
                        }
                        outarch.flags.gcc = obody;
        
                        let get_var = "flags_ld";
                        let obj_var = com_expect!(lexer, build.vars.get(get_var), "Error: Expected {get_var} for architecture build but found nothing!");
                        com_assert!(obj_var,obj_var.typ.is_array(), "Error: expected {get_var} to be an array but found nothing!");
                        let obj_var_ar = obj_var.typ.unwrap_array();
                        let mut obody: Vec<String> = Vec::with_capacity(obj_var_ar.len());
                        for obj in obj_var_ar {
                            com_assert!(obj_var, obj.is_string(), "Error: Cannot have flags to {get_var} which are not strings");
                            obody.push(obj.unwrap_string().clone())
                        }
                        outarch.flags.ld = obody;
                        //outarch.obj_extension = obj_var.typ.unwrap_string().clone();
                        
                        outarch
                    } else {
                        let oarch_arch = Architectures.get(&oarch);
                        if oarch_arch.is_none() {
                            update_progress!("   {}Failed{} Unknown architecture {}",RED(),oarch,RESET());
                        }
                        let mut oarch = oarch_arch.unwrap().clone();
                        if let Some(flags) = build.vars.get("flags_gcc") {
                            com_assert!(flags, flags.typ.is_array(), "Error: expected flags_gcc to be array but found other!");
                            let flags_var_ar = flags.typ.unwrap_array();
                            oarch.flags.gcc.extend(flags_var_ar.iter().map(
                                    |v| {
                                com_assert!(flags, v.is_string(), "Expected string in flags, found other");
                                v.unwrap_string().clone()
                            }));
                        }
                        oarch
                    };
                    let out_obj_path = intpath.clone()+"/main."+&arc.obj_extension;
                    {
                        if let Some(build_files) = build.vars.get("build") {
                            com_assert!(build_files,build_files.typ.is_array(), "Error: Expected build to be an array but found something else");
                            let vals = build_files.typ.unwrap_array();
                            let intpath = build.vars.get("intpath").expect("Error: Expected entry but found nothing! Cannot have build without entry specification for now (libs are yet to be implemented for lighthouse)");
                            com_assert!(intpath, intpath.typ.is_string(), "Error: Expected value of entry to be string but found other");
                            for val in vals.iter() {
                                com_assert!(build_files, val.is_string(), "Error: All elements of build must be strings!");
                                let val = val.unwrap_string();
                                let v_ = PathBuf::from(val);
                                let val = v_.file_stem().unwrap();
                                let int_rep = PathBuf::from(val).with_extension(&arc.obj_extension);
                                let int_rep_str = int_rep.to_str().unwrap_or_default();
                                let asm_rep = PathBuf::from(val).with_extension("asm");
                                let asm_rep_str = asm_rep.to_str().unwrap_or_default();
                                //let exe_rep = Path
                                let mut oargs = arc.flags.nasm.clone();
                                oargs.extend(vec![intpath.typ.unwrap_string().clone()+"/"+asm_rep_str,"-o".to_owned(), intpath.typ.unwrap_string().clone()+"/"+int_rep_str]);
                                update_progress!("   {}Running nasm {}{}",LIGHT_BLUE(),oargs.join(" "),RESET());
                                let cmd = Command::new("nasm").args(oargs).spawn();
                                if cmd.is_err() {
                                    update_progress!("   {}Error: could not run nasm command{}\n",RED(),RESET());
                                    exit(1);
                                }
                                let mut cmd = cmd.unwrap();
                                let status = cmd.wait();
                                if status.is_err() {
                                    update_progress!("   {}Error: could not recieve any information from nasm command{}\n",RED(),RESET());
                                    exit(1);
                                }
                                let ostatus = status.unwrap().code().unwrap_or(1);
                                if ostatus == 0 {
                                    update_progress!("   {}Finished{} built code with nasm successfully",GREEN(),RESET());
                                }
                                else {
                                    update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}\n",RED(),RESET(),ostatus);
                                    exit(ostatus)
                                }
                            }
                        }
                        let mut oargs = arc.flags.nasm.clone();
                        oargs.extend(vec![intpath.clone()+"/main.asm","-o".to_owned(), out_obj_path.clone()]);
                        update_progress!("   {}Running nasm {}{}",LIGHT_BLUE(),oargs.join(" "),RESET());
                        let cmd = Command::new("nasm").args(oargs).spawn();
                        if cmd.is_err() {
                            update_progress!("   {}Error: could not run nasm command{}\n",RED(),RESET());
                            exit(1);
                        }
                        let mut cmd = cmd.unwrap();
                        let status = cmd.wait();
                        if status.is_err() {
                            update_progress!("   {}Error: could not recieve any information from nasm command{}\n",RED(),RESET());
                            exit(1);
                        }
                        let ostatus = status.unwrap().code().unwrap_or(1);
                        if ostatus == 0 {
                            update_progress!("   {}Finished{} built code with nasm successfully",GREEN(),RESET());
                        }
                        else {
                            update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}\n",RED(),RESET(),ostatus);
                            exit(ostatus)
                        }
                    }
                    let obuf = binpath.clone()+"/main."+&arc.exe_extension;
                    let linker = build.vars.get("linker");
                    let linker = if let Some(linker) = linker {
                        com_assert!(linker, linker.typ.is_string(), "Error: Expected linker type to be a string but found other!");
                        linker.typ.unwrap_string().clone()
                    } else {
                        "gcc".to_owned()
                    };
                    match linker.as_str() {
                        "gcc" => {
                            let mut oargs = arc.flags.gcc.clone();
                            if let Some(build_files) = build.vars.get("build") {
                                com_assert!(build_files,build_files.typ.is_array(), "Error: Expected build to be an array but found something else");
                                let vals = build_files.typ.unwrap_array();
                                let intpath = build.vars.get("intpath").expect("Error: Expected entry but found nothing! Cannot have build without entry specification for now (libs are yet to be implemented for lighthouse)");
                                com_assert!(intpath, intpath.typ.is_string(), "Error: Expected value of entry to be string but found other");
                                for val in vals.iter() {
                                    com_assert!(build_files, val.is_string(), "Error: All elements of build must be strings!");
                                    let val = val.unwrap_string();
                                    let v_ = PathBuf::from(val);
                                    let val = v_.file_stem().unwrap();
                                    let int_rep = PathBuf::from(val).with_extension(&arc.obj_extension);
                                    let int_rep_str = int_rep.to_str().unwrap_or_default();
                                    oargs.push(intpath.typ.unwrap_string().clone()+"/"+int_rep_str);
                                }
                            }
                            oargs.extend(vec![out_obj_path.clone(),"-o".to_owned(), obuf.clone()]);
                            update_progress!("   {}Running gcc {}{}",LIGHT_BLUE(),oargs.join(" "),RESET());
                            let cmd = Command::new("gcc").args(oargs).spawn();
                            if cmd.is_err() {
                                update_progress!("   {}Error: could not run gcc command{}\n",RED(),RESET());
                                exit(1);
                            }
                            let mut cmd = cmd.unwrap();
                            let status = cmd.wait();
                            if status.is_err() {
                                update_progress!("   {}Error: could not recieve any information from gcc command{}\n",RED(),RESET());
                                exit(1);
                            }
                            let ostatus = status.unwrap().code().unwrap_or(1);
                            if ostatus == 0 {
                                update_progress!("   {}Finished{} built code with gcc successfully",GREEN(),RESET());
                            }
                            else {
                                update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}\n",RED(),RESET(),ostatus);
                                exit(ostatus)
                            }
                        }
                        "ld" => {
                            let mut oargs = arc.flags.ld.clone();
                            //let pbuf = PathBuf::from(out_obj_path.clone());
                            //let obuf = pbuf.with_extension("exe").to_str().unwrap().to_owned();
                            if let Some(build_files) = build.vars.get("build") {
                                com_assert!(build_files,build_files.typ.is_array(), "Error: Expected build to be an array but found something else");
                                let vals = build_files.typ.unwrap_array();
                                let intpath = build.vars.get("intpath").expect("Error: Expected entry but found nothing! Cannot have build without entry specification for now (libs are yet to be implemented for lighthouse)");
                                com_assert!(intpath, intpath.typ.is_string(), "Error: Expected value of entry to be string but found other");
                                for val in vals.iter() {
                                    com_assert!(build_files, val.is_string(), "Error: All elements of build must be strings!");
                                    let val = val.unwrap_string();
                                    let v_ = PathBuf::from(val);
                                    let val = v_.file_stem().unwrap();
                                    let int_rep = PathBuf::from(val).with_extension(&arc.obj_extension);
                                    let int_rep_str = int_rep.to_str().unwrap_or_default();
                                    oargs.push(intpath.typ.unwrap_string().clone()+"/"+int_rep_str);
                                }
                            }
                            oargs.extend(vec![out_obj_path.clone(),"-o".to_owned(), obuf.clone()]);
                            update_progress!("   {}Running ld {}{}",LIGHT_BLUE(),oargs.join(" "),RESET());
                            let cmd = Command::new("ld").args(oargs).spawn();
                            if cmd.is_err() {
                                update_progress!("   {}Error: could not run ld command{}\n",RED(),RESET());
                                exit(1);
                            }
                            let mut cmd = cmd.unwrap();
                            let status = cmd.wait();
                            if status.is_err() {
                                update_progress!("   {}Error: could not recieve any information from ld command{}\n",RED(),RESET());
                                exit(1);
                            }
                            let ostatus = status.unwrap().code().unwrap_or(1);
                            if ostatus == 0 {
                                update_progress!("   {}Finished{} built code with ld successfully",GREEN(),RESET());
                            }
                            else {
                                update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}\n",RED(),RESET(),ostatus);
                                exit(ostatus)
                            }
                        }
                        _ => {
                            update_progress!("   {}Failed Lighthouse does not support linker '{}'{}\n",RED(),linker,RESET());
                            exit(1)
                        }
                    }
                    update_progress!("   {}Finished{} Compiled program sucessfully",GREEN(),RESET());
                    {
                        //println!("Args: {:?}",args);
                        let cmd = Command::new(obuf).args(args).stdout(Stdio::inherit()).stdin(Stdio::inherit()).stderr(Stdio::inherit()).spawn();
                        println!();
                        if cmd.is_err() {
                            update_progress!("   {}Error: could not run program{}\n",RED(),RESET());
                            exit(1);
                        }
                        let mut cmd = cmd.unwrap();
                        let status = cmd.wait();
                        if status.is_err() {
                            update_progress!("   {}Error: could not recieve any information from program{}\n",RED(),RESET());
                            exit(1);
                        }
                        let ostatus = status.unwrap().code().unwrap_or(1);
                        if ostatus == 0 {
                            println!("   {}Program exited with {}{}",GREEN(),ostatus,RESET())
                        }
                        else{
                            println!("   {}Program exited with {}{}",RED(),ostatus,RESET())
                        }
                    }
                }
                _ => {
                    todo!("Cannot build for target {} yet",target_typ);
                }
            }
            
        
            
            //Command::new("nasm").args(vec!["-f"])
            
        }
        "lib" => {
            update_progress!("   {}Verifying package{}",LIGHT_BLUE(),RESET());
            let cdir = current_dir().unwrap_or_default();
            let cdirstr = cdir.to_str().unwrap_or_default().to_owned();
            let res = verify_package_exists(cdirstr);
            if res.is_err() {
                let err = res.unwrap_err();
                update_progress!("   {}Error: Invalid package: {}{}",RED(),err,RESET());
                exit(1);
            }
            update_progress!("   {}Package verified{}",GREEN(),RESET());
            // let mut i: usize = 0;
            // while i < args.len() {
            //     match args[i].as_str() {
            //         "add" => {
            //             let res = update_progress_expect!(args.get(i+1), "   {}Expected type or path but found nothing{}",RED(),RESET());
            //             match res {
            //                 "local" => {

            //                 }
            //             }
            //         }
            //         _ => {
            //             update_progress!("    {}Unknown flag {}{}",RED(),args[i],RESET());
            //             exit(1);
            //         }
            //     }
            //     i+=1;
            // }
            todo!("This")
        }
        "init" => {
            update_progress!("   {}Checking for existing package{}",GREEN(),RESET());
            let cdir = current_dir().unwrap_or_default();
            let cdirstr = cdir.to_str().unwrap_or_default().to_owned();
            let cdirname = cdir.file_name().unwrap_or(OsStr::new("SOPL")).to_str().unwrap_or_default().to_owned();
            
            struct InitOptions {
                package_name: String,
                package_type: PackageType,
            }
            impl InitOptions {
                fn new() -> Self {
                    Self { package_name: String::new(), package_type: PackageType::BIN }
                }
            }
            let mut options = InitOptions::new();
            options.package_name = cdirname.clone();
            {
                let mut i: usize = 0;
                while i < args.len() {
                    match args[i].as_str() {
                        "--name" => {
                            i+=1;
                            options.package_name = args[i].clone()
                        }
                        "--type" => {
                            i+=1;
                            options.package_type = match args[i].as_str() {
                                "bin" => {
                                    PackageType::BIN
                                }
                                "lib" => {
                                    PackageType::LIB
                                }
                                _ => panic!("Unknown package type {}",args[i])
                            }
                        }
                        _ => panic!("Unknown flag {}",args[i])
                    }
                    i += 1;

                }
            }
            // let package_name = if let Some(name) = remove_arg_if(&mut args) {
            //     if name == "bin" || name == "lib" {
            //         cdirname.clone()
            //     }
            //     else {
            //         name
            //     }
            // }
            // else {
            //     cdirname.clone()
            // };

            
            if !can_create_package(cdirstr.clone()) {
                update_progress!("    {}Error: cannot reinitialize a package! Package already exists{}\n",RED(),RESET());
                exit(1)
            }

            update_progress!("   {}Creating src folder{}",LIGHT_BLUE(),RESET());
            let res = create_dir("./src");
            if res.is_err() {
                update_progress!("   {}Could not create src folder{}\nReason: {}{}{}\n",RED(),RESET(),RED(),res.unwrap_err().to_string(),RESET());
                exit(1);
            }
            update_progress!("   {}Creating lighthouse.cfg file{}",LIGHT_BLUE(),RESET());
            let res = File::create("./lighthouse.cfg");
            if res.is_err() {
                update_progress!("   {}Could not create lighthouse.cfg file{}",RED(),RESET());
                exit(1);
            }
            let mut cfg_f = res.unwrap();
            writeln!(&mut cfg_f, "## Config supports comments :D").unwrap();
            writeln!(&mut cfg_f, "name=\"{}\"",options.package_name).unwrap();
            writeln!(&mut cfg_f, "intpath=\"{}\"","./output/int").unwrap();
            writeln!(&mut cfg_f, "binpath=\"{}\"","./output/bin").unwrap();
            match options.package_type {
                PackageType::BIN => {
                    writeln!(&mut cfg_f, "entry=\"{}\"","./src/main.spl").unwrap();
                    writeln!(&mut cfg_f, "type=\"bin\"").unwrap()
                }
                PackageType::LIB => {
                    writeln!(&mut cfg_f, "type=\"lib\"").unwrap()
                },
                _ => {}
            }
            update_progress!("   {}Working on .gitignore file{}",LIGHT_BLUE(),RESET());
            {
                let gitpath = PathBuf::from(".gitignore");
                let mut res = if gitpath.exists() {
                    let res = OpenOptions::new().append(true).open(gitpath);
                    if res.is_err() {
                        update_progress!("   {}Could not open .gitignore file{}",RED(),RESET());
                        exit(1);
                    }
                    res.unwrap()
                }
                else {
                    let res = File::create(".gitignore");
                    if res.is_err() {
                        update_progress!("   {}Could not create .gitignore file{}",RED(),RESET());
                        exit(1);
                    }
                    res.unwrap()
                };
                writeln!(&mut res, "/output/").unwrap();
            }
            
            
            match options.package_type {
            PackageType::BIN => {
                update_progress!("   {}Creating src/main.spl file{}",LIGHT_BLUE(),RESET());
                let res = File::create("./src/main.spl");
                if res.is_err() {
                    update_progress!("   {}Could not create main.spl file{}",RED(),RESET());
                    exit(1);
                }
                let mut mainspl_f = res.unwrap();
                writeln!(&mut mainspl_f, "extern \"C\" puts(*char);").unwrap();
                writeln!(&mut mainspl_f, "func main(){{").unwrap();
                writeln!(&mut mainspl_f, "   puts(\"Hello World!\")").unwrap();
                writeln!(&mut mainspl_f, "}}").unwrap();
            }
            PackageType::LIB => {
                update_progress!("   {}Creating src/lib.spl file{}",LIGHT_BLUE(),RESET());
                let res = File::create("./src/lib.spl");
                if res.is_err() {
                    update_progress!("   {}Could not create lib.spl file{}",RED(),RESET());
                    exit(1);
                }
                let mut mainspl_f = res.unwrap();
                writeln!(&mut mainspl_f, "extern \"C\" puts(*char);").unwrap();
                writeln!(&mut mainspl_f, "func my_awesome_func(){{").unwrap();
                writeln!(&mut mainspl_f, "   puts(\"Hello World!\")").unwrap();
                writeln!(&mut mainspl_f, "}}").unwrap();
            }
            }
            update_progress!("   {}Creating output folder{}",LIGHT_BLUE(),RESET());
            let res = create_dir("./output");
            if res.is_err() {
                update_progress!("   {}Could not create output folder{}\nReason: {}{}{}\n",RED(),RESET(),RED(),res.unwrap_err().to_string(),RESET());
                exit(1);
            }
            update_progress!("   {}Creating int folder{}",LIGHT_BLUE(),RESET());
            let res = create_dir("./output/int");
            if res.is_err() {
                update_progress!("   {}Could not create int folder{}\nReason: {}{}{}\n",RED(),RESET(),RED(),res.unwrap_err().to_string(),RESET());
                exit(1);
            }
            update_progress!("   {}Creating bin folder{}",LIGHT_BLUE(),RESET());
            let res = create_dir("./output/bin");
            if res.is_err() {
                update_progress!("   {}Could not create bin folder{}\nReason: {}{}{}\n",RED(),RESET(),RED(),res.unwrap_err().to_string(),RESET());
                exit(1);
            }
             
            update_progress!("   {}Initializing package{} \"{}\"",LIGHT_BLUE(),RESET(),options.package_name);
            update_progress!("   {}Finished{} Initialized package \"{}\"",GREEN(),RESET(),options.package_name);
        }
        _ => panic!("Unknown command: \"{}\"",command)
    }
    
}



/*
local_dependencies=["../../stdlib"]            // you would pass the -i flag with this array and it will make it easier to include (you can just do include "stdlib.spl")
                                               // it also builds the package and if it has anything under "build" it also links with it

*/
