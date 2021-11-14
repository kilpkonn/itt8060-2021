




module FileSystem

type Path = string list

type FsTree =
    {
      name: string
      children: FsTree list
    }

val isEmpty: fs: FsTree -> bool

val show: fs: FsTree -> Path list

val create: p: Path -> fs: FsTree -> FsTree

val delete: p: Path -> fs: FsTree -> FsTree


