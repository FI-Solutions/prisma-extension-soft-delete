import { Config } from "./types";
export declare function createSoftDeleteExtension({ models, defaultConfig, dmmf, }: Config): (client: any) => import("@prisma/client/extension").PrismaClientExtends<import("@prisma/client/runtime/library").InternalArgs<{}, {}, {}, {}> & import("@prisma/client/runtime/library").DefaultArgs>;
