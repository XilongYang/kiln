#!/bin/sh
set -eu

if [ "$#" -ne 4 ]; then
  echo "usage: $0 <repo-root> <case-name> <dataset-rel-path> <hardlink|copy>" >&2
  exit 1
fi

REPO_ROOT="$1"
CASE_NAME="$2"
DATASET_REL_PATH="$3"
COPY_MODE="$4"

WORK_ROOT="$REPO_ROOT/.cache/PT/workspaces/$CASE_NAME"
DATASET_SRC="$REPO_ROOT/$DATASET_REL_PATH"

if [ ! -d "$DATASET_SRC" ]; then
  echo "missing dataset: $DATASET_SRC (run: make perf-data)" >&2
  exit 1
fi

rm -rf "$WORK_ROOT"
mkdir -p "$WORK_ROOT/src"
mkdir -p "$WORK_ROOT/template/component"
mkdir -p "$WORK_ROOT/res/fonts"
mkdir -p "$WORK_ROOT/bin"

case "$COPY_MODE" in
  hardlink)
    cp -al "$DATASET_SRC/." "$WORK_ROOT/src" || cp -a "$DATASET_SRC/." "$WORK_ROOT/src"
    ;;
  copy)
    cp -a "$DATASET_SRC/." "$WORK_ROOT/src"
    ;;
  *)
    echo "invalid copy mode: $COPY_MODE" >&2
    exit 1
    ;;
esac

cat > "$WORK_ROOT/template/post.html" <<'EOF'
<!doctype html>
<html><body><main>$body$</main></body></html>
EOF

cat > "$WORK_ROOT/template/index.html" <<'EOF'
<!doctype html>
<html><body>$posts$</body></html>
EOF

: > "$WORK_ROOT/res/fonts/SourceHanSerifCN-Regular.otf"

cat > "$WORK_ROOT/bin/pandoc" <<'EOF'
#!/bin/sh
in="$1"
out=""
plain=0
while [ "$#" -gt 0 ]; do
  case "$1" in
    -o) shift; out="$1" ;;
    -t) shift; [ "$1" = "plain" ] && plain=1 ;;
  esac
  shift
done
if [ "$plain" -eq 1 ]; then
  cat "$in"
  exit 0
fi
if [ -n "$out" ]; then
  {
    echo "<main>"
    cat "$in"
    echo "</main>"
  } > "$out"
fi
EOF

cat > "$WORK_ROOT/bin/pyftsubset" <<'EOF'
#!/bin/sh
for arg in "$@"; do
  case "$arg" in
    --output-file=*) out="${arg#--output-file=}" ;;
  esac
done
[ -n "$out" ] && : > "$out"
EOF

chmod +x "$WORK_ROOT/bin/pandoc" "$WORK_ROOT/bin/pyftsubset"
