use super::types::ModuleInterface;
use crate::ast::{self, expression::Expressions, span::Spans, ModuleFullName};
use crate::source::{self, Source, Sources, SourcesData};
use crate::strings::Strings;
use crate::token::Tokens;
use derive_more::{From, Into};
use fnv::FnvHashMap;
use std::fmt::Write;
use typed_index_collections::TiVec;

type HashMap<K, V> = FnvHashMap<K, V>;

#[derive(Debug, From, Into, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ModuleIndex(usize);
type Modules<T> = TiVec<ModuleIndex, T>;

#[derive(Debug)]
pub struct State {
    pub strings: Strings,
    pub module_to_source_idx: Modules<source::Index>,
    pub module_name_to_module_idx: HashMap<ModuleFullName, ModuleIndex>,
    pub sources: Sources,
    pub tokens: SourcesData<Tokens>,
    pub spans: SourcesData<Spans>,
    pub expressions: SourcesData<Expressions>,
    pub modules: Modules<ast::Module>,
    pub types: Modules<Option<ModuleInterface>>,
}

impl State {
    pub fn new() -> Self {
        Self {
            sources: Sources::new(),
            tokens: SourcesData::new(),
            spans: SourcesData::new(),
            expressions: SourcesData::new(),
            modules: Modules::new(),
            types: Modules::new(),
            strings: Strings::new(),
            module_to_source_idx: Modules::new(),
            module_name_to_module_idx: HashMap::default(),
        }
    }

    pub fn add_source(&mut self, source: Source) -> source::Index {
        self.sources.push(source);
        self.tokens.push(Tokens::new());
        self.spans.push(Spans::new());
        self.expressions.push(Expressions::new());

        // Return the newly created file index, sources and tokens and spans should always be in
        // sync with file indexes
        debug_assert_eq!(self.sources.len(), self.tokens.len());
        debug_assert_eq!(self.sources.len(), self.spans.len());
        debug_assert_eq!(self.sources.len(), self.expressions.len());
        self.sources.last_key().unwrap()
    }

    pub fn add_module_ast(
        &mut self,
        source_idx: source::Index,
        module_ast: ast::Module,
    ) -> ModuleIndex {
        let module_name = module_ast.name.full_name;
        self.modules.push(module_ast);
        self.types.push(None);
        self.module_to_source_idx.push(source_idx);
        let module_idx = self.modules.last_key().unwrap();
        self.module_name_to_module_idx
            .insert(module_name, module_idx);

        // Return the newly created module index, asts and types should always be in sync with
        // module indexes
        debug_assert_eq!(self.modules.len(), self.types.len());
        debug_assert_eq!(self.modules.len(), self.module_to_source_idx.len());
        module_idx
    }

    pub fn has_ast(&self, name: ModuleFullName) -> bool {
        self.get_module_idx_with_name(name)
            .map(|_| true)
            .unwrap_or(false)
    }

    pub fn get_module_idx_with_name(&self, name: ModuleFullName) -> Option<ModuleIndex> {
        match self.module_name_to_module_idx.get(&name) {
            Some(idx) => Some(*idx),
            None => None,
        }
    }

    pub fn has_types(&self, name: ModuleFullName) -> bool {
        self.get_types_for_name(name).is_some()
    }

    pub fn get_types_for_name(&self, name: ModuleFullName) -> Option<&ModuleInterface> {
        if let Some(idx) = self.get_module_idx_with_name(name) {
            self.types[idx].as_ref()
        } else {
            None
        }
    }

    pub fn types_to_string(&self, out: &mut String) {
        for (i, (module_idx, interface)) in self.types.iter_enumerated().enumerate() {
            if i > 0 {
                out.push_str("\n\n\n");
            }
            let name_sym = self.modules[module_idx].name.full_name;
            let name = self.strings.resolve(name_sym);
            let interface = interface
                .as_ref()
                .map(|interface| interface.to_string(&self.strings))
                .unwrap_or(String::new());
            write!(out, "module {name}\n\n{interface}").unwrap();
        }
    }
}
