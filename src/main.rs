#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unreachable_patterns)]

use core::panic;
use std::{env::{self, current_dir}, thread, time, io::{Write, stderr}, ffi::OsStr, path::{PathBuf, Path}, process::{exit, Command, Stdio}, fs::{create_dir, File, OpenOptions, self}, rc::Rc, fmt::format, collections::HashMap};
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
#[derive(PartialEq)]
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
#[derive(PartialEq)]
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
#[derive(PartialEq)]
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
struct Token {
    loc: ProgramLocation,
    typ: TokenType
}
impl Token {
    fn loc_display(&self) -> String {
        self.loc.loc_display()
    }
}
#[derive(Debug)]
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
        if chr == '\\' {
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
        if !c.is_alphabetic() && c != '_'{
            return None;
        }
        while self.is_not_empty() && c.is_alphanumeric() || c=='_'{
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
                c = self.cchar();
                let mut outstr: String = String::new();
                while self.is_not_empty() && (shouldIgnoreNext || c != '\"'){
                    c = self.cchar_s()?;
                    if shouldIgnoreNext {
                        shouldIgnoreNext = false
                    }
                    if c == '\\' {
                        shouldIgnoreNext = true
                    }
                    outstr.push(c);
                    self.cursor+=1;
                }
                outstr.pop();
                self.loc.character+=2+outstr.len();
                return Some(Token { loc: self.loc.clone(), typ: TokenType::StringType(outstr) });
            }
            '[' | ']' => {
                self.cursor += 1;
                self.loc.character += 1;                
                let tmp = c.to_string();
                return Some(Token { loc: self.loc.clone(), typ: TokenType::IntrinsicType(IntrinsicType::from_str(tmp.as_str()).expect("Unhandled :(")) });
            }
            _ => {
                if c.is_alphabetic() || c == '_' {
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

const RED: &str = "\x1b[31;1m";
const GREEN: &str = "\x1b[32;1m";
const LIGHT_BLUE: &str= "\x1b[96;1m";
const RESET: &str = "\x1b[0m";
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
    pinfo.cfg = PathBuf::from(dir.clone()+"\\lighthouse.cfg");
    pinfo.src_folder = PathBuf::from(dir.clone()+"\\src");
    !pinfo.cfg.exists() && !pinfo.src_folder.exists()
}
fn verify_package_exists(dir: String) -> Result<PackageInfo, String> {
    let mut pinfo = PackageInfo::new();
    pinfo.cfg = PathBuf::from(dir.clone()+"\\lighthouse.cfg");
    if !pinfo.cfg.exists() {
        return Err("cfg does not exist".to_owned());
    }
    if !pinfo.cfg.is_file()  {
        return Err("cfg was not a file".to_owned());
    }
    pinfo.src_folder = PathBuf::from(dir.clone()+"\\src");
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
                let ntok = par_expect!(lexer,lexer.next(),"Error: Expected setop after word but found nothing");
                par_assert!(ntok,ntok.typ==TokenType::SETOp(SETOp::SET), "Error: Expected setop but found {}",ntok.typ.to_string());
                let ntok = par_expect!(lexer,lexer.next(),"Error: Expected value after setop but found nothing");
                match ntok.typ {
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
                                TokenType::NumberType(v) => {
                                    array_body.push(CfgVarType::NUMBER(v))
                                }
                                TokenType::StringType(v) => {
                                    array_body.push(CfgVarType::STRING(v))
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
fn build_package() -> CfgBuild {
    update_progress!("   {}Verifying package{}",LIGHT_BLUE,RESET);
    let cdir = current_dir().unwrap_or_default();
    let cdirstr = cdir.to_str().unwrap_or_default().to_owned();
    let res = verify_package_exists(cdirstr);
    if res.is_err() {
        let err = res.unwrap_err();
        update_progress!("   {}Error: Invalid package: {}{}",RED,err,RESET);
        exit(1);
    }
    let res = res.unwrap();
    update_progress!("   {}Package verified{}",GREEN,RESET);
    let f = fs::read_to_string(res.cfg);
    if f.is_err() {
        let err = f.unwrap_err();
        update_progress!("   {}Error: Could not open config: {}{}",RED,err,RESET);
        exit(1);
    }
    let cfgstr = f.unwrap();
    let mut lexer = CfgLexer::new(cfgstr);
    update_progress!("    {}Parsing lighthouse.cfg{}",LIGHT_BLUE,RESET);
    lexer.loc.file = Rc::new("lighthouse.cfg".to_owned());
    let build = parse_tokens_to_build(&mut lexer);
    update_progress!("   {}Parsed lighthouse.cfg successfully{}",GREEN,RESET);
    {
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
            update_progress!("   {}Error: entry path {} does not exist (entry defined {}){}\n",RED,entry.typ.unwrap_string(),entry.loc_display(),RESET);
            exit(1);
        }
        if !entry_path.is_file() {
            update_progress!("   {}Error: entry path {} isn't a file (entry defined {}){}\n",RED,entry.typ.unwrap_string(),entry.loc_display(),RESET);
            exit(1);
        }
        let int_rep = PathBuf::from(entry_path.file_stem().unwrap()).with_extension("asm");
        let int_rep_str = int_rep.to_str().unwrap_or_default();
        let intpath = build.vars.get("intpath").expect("Error: Expected entry but found nothing! Cannot have build without entry specification for now (libs are yet to be implemented for lighthouse)");
        com_assert!(intpath, intpath.typ.is_string(), "Error: Expected value of entry to be string but found other");
        
        oargs.extend(vec!["-o".to_owned(),intpath.typ.unwrap_string().clone()+"\\"+int_rep_str]);
        update_progress!("   {}Running sopl {}{}\n",LIGHT_BLUE,oargs.join(" "),RESET);
        let cmd = Command::new("sopl").args(oargs).stdout(Stdio::inherit()).stdin(Stdio::inherit()).stderr(Stdio::inherit()).spawn();
        println!();
        if cmd.is_err() {
            update_progress!("   {}Error: could not run sopl command{}\n",RED,RESET);
            exit(1);
        }
        let mut cmd = cmd.unwrap();
        let status = cmd.wait();
        if status.is_err() {
            update_progress!("   {}Error: could not recieve any information from sopl command{}\n",RED,RESET);
            exit(1);
        }
        let status = status.unwrap();
        let ostatus = status.code().unwrap_or(1);
        if ostatus == 0 {
            update_progress!("   {}Finished{} built code successfully",GREEN,RESET);
        }
        else {
            update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}",RED,RESET,ostatus);
            exit(ostatus)
        }
    }
    build
}
#[derive(Debug,Clone)]
struct ArcFlags {
    nasm: Vec<String>,
    gcc:  Vec<String>
}
impl ArcFlags {
    fn new() -> Self {
        Self { nasm: Vec::new(), gcc: Vec::new() }
    }
}
#[derive(Debug,Clone)]
struct LHArchitecture {
    flags: ArcFlags,
    obj_extension: String,
    exe_extension: String
}

fn main() {
    let mut Architectures: HashMap<String, LHArchitecture> = HashMap::new();
    
    Architectures.insert("windows_x86_64".to_owned(), LHArchitecture{flags: ArcFlags { nasm: vec!["-f".to_string(),"win64".to_string()], gcc: vec!["-m64".to_string()] }, obj_extension: "obj".to_owned(), exe_extension: "exe".to_owned()});
    Architectures.insert("windows_x86".to_owned(),    LHArchitecture{flags: ArcFlags { nasm: vec!["-f".to_string(),"win32".to_string()], gcc: vec!["-m32".to_string()] }, obj_extension: "obj".to_owned(), exe_extension: "exe".to_owned()});
    Architectures.insert("linux_x86_64".to_owned(),   LHArchitecture{flags: ArcFlags { nasm: vec!["-f".to_string(),"elf64".to_string()], gcc: vec!["-m64".to_string()] }, obj_extension: "o"  .to_owned(), exe_extension: "".to_owned()});
    Architectures.insert("linux_x86".to_owned(),      LHArchitecture{flags: ArcFlags { nasm: vec!["-f".to_string(),"elf64".to_string()], gcc: vec!["-m32".to_string()] }, obj_extension: "o"  .to_owned(), exe_extension: "".to_owned()});
    //short calls
    Architectures.insert("win_x86_64".to_owned(), Architectures.get("windows_x86_64").unwrap().clone());
    Architectures.insert("win_x86".to_owned(), Architectures.get("windows_x86").unwrap().clone());
    let mut args: Vec<_> = env::args().collect();
    
    let _program = remove_arg_if(&mut args).expect("Error: expected program but found nothing");
    let command = remove_arg_if(&mut args).expect("Error: expected command but found nothing");
    match command.as_str() {
        "verify" => {
            update_progress!("   {}Verifying package{}",LIGHT_BLUE,RESET);
            let cdir = current_dir().unwrap_or_default();
            let cdirstr = cdir.to_str().unwrap_or_default().to_owned();
            let res = verify_package_exists(cdirstr);
            if res.is_err() {
                let err = res.unwrap_err();
                update_progress!("   {}Error: Invalid package: {}{}",RED,err,RESET);
                exit(1);
            }
            update_progress!("   {}Package verified{}",GREEN,RESET);
        }
        "build" => {
            build_package();
        }
        "run" => {
            let build = build_package();
            
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
                    if let Some(arc) = Architectures.get(&("".to_owned()+env::consts::OS+"_"+env::consts::ARCH)) {
                        let out_obj_path = intpath.clone()+"\\main."+&arc.obj_extension;
                        {
                            let mut oargs = arc.flags.nasm.clone();
                            oargs.extend(vec![intpath.clone()+"\\main.asm","-o".to_owned(), out_obj_path.clone()]);
                            update_progress!("   {}Running nasm {}{}",oargs.join(" "),LIGHT_BLUE,RESET);
                            let cmd = Command::new("nasm").args(oargs).spawn();
                            if cmd.is_err() {
                                update_progress!("   {}Error: could not run nasm command{}\n",RED,RESET);
                                exit(1);
                            }
                            let mut cmd = cmd.unwrap();
                            let status = cmd.wait();
                            if status.is_err() {
                                update_progress!("   {}Error: could not recieve any information from nasm command{}\n",RED,RESET);
                                exit(1);
                            }
                            let ostatus = status.unwrap().code().unwrap_or(1);
                            if ostatus == 0 {
                                update_progress!("   {}Finished{} built code with nasm successfully",GREEN,RESET);
                            }
                            else {
                                update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}",RED,RESET,ostatus);
                                exit(ostatus)
                            }
                        }
                        let obuf = binpath.clone()+"\\main."+&arc.exe_extension;
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
                                //let pbuf = PathBuf::from(out_obj_path.clone());
                                //let obuf = pbuf.with_extension("exe").to_str().unwrap().to_owned();
                                
                                oargs.extend(vec![out_obj_path.clone(),"-o".to_owned(), obuf.clone()]);
                                update_progress!("   {}Running gcc {}{}",oargs.join(" "),LIGHT_BLUE,RESET);
                                let cmd = Command::new("gcc").args(oargs).spawn();
                                if cmd.is_err() {
                                    update_progress!("   {}Error: could not run gcc command{}\n",RED,RESET);
                                    exit(1);
                                }
                                let mut cmd = cmd.unwrap();
                                let status = cmd.wait();
                                if status.is_err() {
                                    update_progress!("   {}Error: could not recieve any information from gcc command{}\n",RED,RESET);
                                    exit(1);
                                }
                                let ostatus = status.unwrap().code().unwrap_or(1);
                                if ostatus == 0 {
                                    update_progress!("   {}Finished{} built code with gcc successfully",GREEN,RESET);
                                }
                                else {
                                    update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}",RED,RESET,ostatus);
                                    exit(ostatus)
                                }
                            }
                            _ => {
                                update_progress!("   {}Failed Lighthouse does not support linker '{}'{}\n",RED,linker,RESET);
                                exit(1)
                            }
                        }
                        
                        {
                            let cmd = Command::new(obuf).stdout(Stdio::inherit()).stdin(Stdio::inherit()).stderr(Stdio::inherit()).spawn();
                            println!();
                            if cmd.is_err() {
                                update_progress!("   {}Error: could not run program{}\n",RED,RESET);
                                exit(1);
                            }
                            let mut cmd = cmd.unwrap();
                            let status = cmd.wait();
                            if status.is_err() {
                                update_progress!("   {}Error: could not recieve any information from program{}\n",RED,RESET);
                                exit(1);
                            }
                            let ostatus = status.unwrap().code().unwrap_or(1);
                            if ostatus == 0 {
                                println!("   {}Program exited with {}{}",GREEN,ostatus,RESET)
                            }
                            else{
                                println!("   {}Program exited with {}{}",RED,ostatus,RESET)
                            }
                            //if ostatus == 0 {
                            //    update_progress!("   {}Finished{} ran code with gcc successfully",GREEN,RESET);
                            //}
                            //else {
                            //    update_progress!("   {}Failed{} wasn't able to compile, gotten status code: {}",RED,RESET,ostatus);
                            //    exit(ostatus)
                            //}
                            
                        }
                    }
                    else {
                        todo!("We're very sorry but we don't support currently your OS architecture {} nor do we have custom architectures for lighthouse yet :(",&("".to_owned()+env::consts::OS+"_"+env::consts::ARCH));
                    }
                    //Command::new("nasm").args(vec!["-f"])
                }
                _ => {
                    todo!("Cannot build for target {} yet",target_typ);
                }
            }
        }
        "init" => {
            update_progress!("   {}Checking for existing package{}",GREEN,RESET);
            let cdir = current_dir().unwrap_or_default();
            let cdirstr = cdir.to_str().unwrap_or_default().to_owned();
            let cdirname = cdir.file_name().unwrap_or(OsStr::new("SOPL")).to_str().unwrap_or_default().to_owned();
            let package_name = 
            if let Some(name) = remove_arg_if(&mut args) {
                name
            }
            else {
                cdirname.clone()
            };

            if !can_create_package(cdirstr.clone()) {
                update_progress!("    {}Error: cannot reinitialize a package! Package already exists{}\n",RED,RESET);
                exit(1)
            }

            update_progress!("   {}Creating src folder{}",LIGHT_BLUE,RESET);
            let res = create_dir(cdirstr.clone()+"\\src");
            if res.is_err() {
                update_progress!("   {}Could not create src folder{}\nReason: {}{}{}\n",RED,RESET,RED,res.unwrap_err().to_string(),RESET);
                exit(1);
            }
            update_progress!("   {}Creating lighthouse.cfg file{}",LIGHT_BLUE,RESET);
            let res = File::create(cdirstr.clone()+"\\lighthouse.cfg");
            if res.is_err() {
                update_progress!("   {}Could not create lighthouse.cfg file{}",RED,RESET);
                exit(1);
            }
            let mut cfg_f = res.unwrap();
            writeln!(&mut cfg_f, "## TODO: Make the default cfg").unwrap();
            writeln!(&mut cfg_f, "name=\"{}\"",package_name).unwrap();
            writeln!(&mut cfg_f, "intpath=\"{}\"",cdirstr.clone()+"\\output\\int").unwrap();
            writeln!(&mut cfg_f, "binpath=\"{}\"",cdirstr.clone()+"\\output\\bin").unwrap();
            writeln!(&mut cfg_f, "entry=\"{}\"",cdirstr.clone()+"\\src\\main.spl").unwrap();
            
            update_progress!("   {}Working on .gitignore file{}",LIGHT_BLUE,RESET);
            {
                let gitpath = PathBuf::from(cdirstr.clone()+"\\.gitignore");
                let mut res = if gitpath.exists() {
                    let res = OpenOptions::new().append(true).open(gitpath);
                    if res.is_err() {
                        update_progress!("   {}Could not open .gitignore file{}",RED,RESET);
                        exit(1);
                    }
                    res.unwrap()
                }
                else {
                    let res = File::create(cdirstr.clone()+"\\.gitignore");
                    if res.is_err() {
                        update_progress!("   {}Could not create .gitignore file{}",RED,RESET);
                        exit(1);
                    }
                    res.unwrap()
                };
                writeln!(&mut res, "/output/").unwrap();
            }
            
            update_progress!("   {}Creating src\\main.spl file{}",LIGHT_BLUE,RESET);
            let res = File::create(cdirstr.clone()+"\\src\\main.spl");
            if res.is_err() {
                update_progress!("   {}Could not create main.spl file{}",RED,RESET);
                exit(1);
            }
            let mut mainspl_f = res.unwrap();
            writeln!(&mut mainspl_f, "extern \"C\" puts(*char);").unwrap();
            writeln!(&mut mainspl_f, "func main(){{").unwrap();
            writeln!(&mut mainspl_f, "   puts(\"Hello World!\")").unwrap();
            writeln!(&mut mainspl_f, "}}").unwrap();
            update_progress!("   {}Creating output folder{}",LIGHT_BLUE,RESET);
            let res = create_dir(cdirstr.clone()+"\\output");
            if res.is_err() {
                update_progress!("   {}Could not create output folder{}\nReason: {}{}{}\n",RED,RESET,RED,res.unwrap_err().to_string(),RESET);
                exit(1);
            }
            update_progress!("   {}Creating int folder{}",LIGHT_BLUE,RESET);
            let res = create_dir(cdirstr.clone()+"\\output\\int");
            if res.is_err() {
                update_progress!("   {}Could not create int folder{}\nReason: {}{}{}\n",RED,RESET,RED,res.unwrap_err().to_string(),RESET);
                exit(1);
            }
            update_progress!("   {}Creating bin folder{}",LIGHT_BLUE,RESET);
            let res = create_dir(cdirstr.clone()+"\\output\\bin");
            if res.is_err() {
                update_progress!("   {}Could not create bin folder{}\nReason: {}{}{}\n",RED,RESET,RED,res.unwrap_err().to_string(),RESET);
                exit(1);
            }
             
            update_progress!("   {}Initializing package{} \"{}\"",LIGHT_BLUE,RESET,package_name);
            update_progress!("   {}Finished{} Initialized package \"{}\"",GREEN,RESET,package_name);
        }
        _ => panic!("Unknown command: \"{}\"",command)
    }
    
}
