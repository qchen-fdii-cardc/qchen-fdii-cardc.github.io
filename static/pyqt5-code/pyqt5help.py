import re
import sys

from PyQt5.QtCore import pyqtSignal, pyqtSlot, QUrl, Qt
from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtWidgets import QTreeWidgetItem, QWidget, QVBoxLayout, QLineEdit, QTreeWidget, QTreeWidgetItemIterator, \
    QApplication, QMainWindow, QDockWidget
import PyQt5
import glob
import importlib


def pyi_file_names(pyqt5_root):
    return glob.glob(f"{pyqt5_root}/*.pyi")


def str_to_class(name, module="PyQt5"):
    package_name, module_name = name.split(".")
    return getattr(sys.modules[module + "." + package_name], module_name)


def root_object_names(pyqt5_root=PyQt5.__path__[0], class_fingerprint="^class ([^\(\)]*)\(sip.wrapper\):$"):
    files = pyi_file_names(pyqt5_root)
    names = []
    for f in files:
        module = f.split("\\")[-1].split(".")[0]
        if module.startswith("Qt"):
            importlib.import_module(f"PyQt5.{module}")
            with open(f) as fid:
                for line in fid:
                    ret = re.match(class_fingerprint, line)
                    if ret is None:
                        continue
                    captures = ret.groups()
                    if len(captures) > 0:
                        names.append(module + "." + captures[0])

    return [str_to_class(n) for n in sorted(names)]


def walk_to_QTreeWdigetItem(self: object, parent: QTreeWidgetItem = None):
    sc = self.__subclasses__()
    item = QTreeWidgetItem(parent)
    item.setText(0, self.__name__)
    item.setText(1, f"{len(self.__dict__)}")
    item.setExpanded(True)
    for c in sc:
        walk_to_QTreeWdigetItem(c, item)
    return item


class TreeWithSearch(QWidget):
    selectItem = pyqtSignal(str)

    def __init__(self, parent=None, classes=None):
        super(TreeWithSearch, self).__init__(parent=parent)
        self.layout = QVBoxLayout(self)

        self.searchBox = QLineEdit()
        self.searchBox.setPlaceholderText("type class name")
        self.classes = QTreeWidget()

        if classes is None:
            classes = root_object_names()
            classes.extend(root_object_names(class_fingerprint="^class ([^\(\)]*)\(PyQt5.sipsimplewrapper\):$"))

            classes.sort(key=lambda iClass: iClass.__name__)

        for i, c in enumerate(classes):
            root = walk_to_QTreeWdigetItem(c)

            self.classes.addTopLevelItem(root)
        self.classes.setHeaderLabels(["name", "funcs"])
        self.classes.setColumnHidden(1, True)
        self.classes.setHeaderHidden(True)
        self.layout.addWidget(self.searchBox)
        self.layout.addWidget(self.classes)

        self.setLayout(self.layout)

        self.classes.clicked.connect(lambda index: self.selectItem.emit(self.classes.itemFromIndex(index).text(0)))

        self.searchBox.textChanged.connect(self._textChanged)

    @pyqtSlot(str)
    def _textChanged(self, name: str):
        # collapse parent nodes
        if name.isspace():
            iterator = QTreeWidgetItemIterator(self.classes, QTreeWidgetItemIterator.HasChildren)
            while (item := iterator.value()) is not None:
                item: QTreeWidgetItem
                item.setExpanded(False)
                iterator += 1
            return

        iterator = QTreeWidgetItemIterator(self.classes, QTreeWidgetItemIterator.All)
        while (item := iterator.value()) is not None:
            item: QTreeWidgetItem
            class_name: str = item.text(0).lower()
            is_show = name.strip().lower() in class_name
            item.setHidden(not is_show)
            # toggle to show and expand all its ancestors
            if is_show:
                p = item
                while p := p.parent():
                    p.setHidden(False)
                    p.setExpanded(True)
            iterator += 1


class QtHelpView(QWebEngineView):

    def __init__(self, parent=None):
        super(QtHelpView, self).__init__(parent)
        self.base_url = "https://doc.qt.io/qt-5"
        self.load(QUrl(self.base_url))

    @pyqtSlot(str)
    def show_class(self, name):
        self.load(QUrl(f"{self.base_url}/{name.lower()}.html"))


if __name__ == '__main__':
    app = QApplication([])

    main_window = QMainWindow()

    tree = TreeWithSearch(main_window)

    dock = QDockWidget()
    dock.setWidget(tree)

    main_window.addDockWidget(Qt.LeftDockWidgetArea, dock)

    # pip install PyQtWebEngine
    view = QtHelpView(main_window)
    tree.selectItem.connect(view.show_class)

    main_window.setCentralWidget(view)

    main_window.resize(1440, 900)
    main_window.show()
    sys.exit(app.exec_())
