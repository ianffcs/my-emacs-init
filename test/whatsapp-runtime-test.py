"""Check the installed worker bundle without starting workers or connecting.

Run after Emacs has built whatsapp: python3 test/whatsapp-runtime-test.py
"""
import importlib.util
from pathlib import Path
import shutil
import tempfile

build = Path(__file__).resolve().parents[1] / "straight/build/whatsapp/scripts"
workers = ("read-worker", "send-worker", "media-worker", "profile-worker")
with tempfile.TemporaryDirectory(prefix="whatsapp-runtime-") as directory:
    isolated = Path(directory)
    # Copy, not symlink: missing dependencies cannot resolve through the repo.
    for name in (*workers, "bridge_protocol"):
        shutil.copyfile(build / (name + ".py"), isolated / (name + ".py"))
    for name in workers:
        spec = importlib.util.spec_from_file_location(name, isolated / (name + ".py"))
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
print("All four installed workers import with only the packaged dependencies.")
