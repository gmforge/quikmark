use criterion::{black_box, criterion_group, criterion_main, Criterion};
use qwikmark::document;

fn bench_document(c: &mut Criterion) {
    c.bench_function("document definition list", |b| {
        b.iter(|| document(black_box(": ab\n alpha\n: 12\n  digit\n  : ivn    roman")))
    });
}

criterion_group!(benches, bench_document);
criterion_main!(benches);
