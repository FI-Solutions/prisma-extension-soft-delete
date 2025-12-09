import { Prisma as PrismaExtensions } from "@prisma/client/extension";
import { withNestedOperations, } from "@roundtreasury/prisma-extension-nested-operations";
import { createAggregateParams, createCountParams, createDeleteManyParams, createDeleteParams, createFindFirstParams, createFindFirstOrThrowParams, createFindManyParams, createFindUniqueParams, createFindUniqueOrThrowParams, createIncludeParams, createSelectParams, createUpdateManyParams, createUpdateParams, createUpsertParams, createWhereParams, createGroupByParams, createContext, } from "./helpers/createParams";
import { modifyReadResult } from "./helpers/modifyResult";
export function createSoftDeleteExtension({ models, defaultConfig = {
    field: "deleted",
    createValue: Boolean,
    allowToOneUpdates: false,
    allowCompoundUniqueIndexWhere: false,
}, dmmf, }) {
    if (!defaultConfig.field) {
        throw new Error("prisma-extension-soft-delete: defaultConfig.field is required");
    }
    if (!defaultConfig.createValue) {
        throw new Error("prisma-extension-soft-delete: defaultConfig.createValue is required");
    }
    const modelConfig = {};
    Object.keys(models).forEach((model) => {
        const modelName = model;
        const config = models[modelName];
        if (config) {
            modelConfig[modelName] =
                typeof config === "boolean" && config ? defaultConfig : config;
        }
    });
    const context = createContext(dmmf);
    const createParamsByModel = Object.keys(modelConfig).reduce((acc, model) => {
        const config = modelConfig[model];
        return {
            ...acc,
            [model]: {
                delete: createDeleteParams.bind(null, context, config),
                deleteMany: createDeleteManyParams.bind(null, context, config),
                update: createUpdateParams.bind(null, context, config),
                updateMany: createUpdateManyParams.bind(null, context, config),
                upsert: createUpsertParams.bind(null, context, config),
                findFirst: createFindFirstParams.bind(null, context, config),
                findFirstOrThrow: createFindFirstOrThrowParams.bind(null, context, config),
                findUnique: createFindUniqueParams.bind(null, context, config),
                findUniqueOrThrow: createFindUniqueOrThrowParams.bind(null, context, config),
                findMany: createFindManyParams.bind(null, context, config),
                count: createCountParams.bind(null, context, config),
                aggregate: createAggregateParams.bind(null, context, config),
                where: createWhereParams.bind(null, context, config),
                include: createIncludeParams.bind(null, context, config),
                select: createSelectParams.bind(null, context, config),
                groupBy: createGroupByParams.bind(null, context, config),
            },
        };
    }, {});
    const modifyResultByModel = Object.keys(modelConfig).reduce((acc, model) => {
        const config = modelConfig[model];
        return {
            ...acc,
            [model]: {
                include: modifyReadResult.bind(null, context, config),
                select: modifyReadResult.bind(null, context, config),
            },
        };
    }, {});
    // before handling root params generate deleted value so it is consistent
    // for the query. Add it to root params and get it from scope?
    return PrismaExtensions.defineExtension((client) => {
        return client.$extends({
            query: {
                $allModels: {
                    // @ts-expect-error - we don't know what the client is
                    $allOperations: withNestedOperations({
                        dmmf,
                        async $rootOperation(initialParams) {
                            var _a, _b;
                            const createParams = (_a = createParamsByModel[initialParams.model || ""]) === null || _a === void 0 ? void 0 : _a[initialParams.operation];
                            if (!createParams)
                                return initialParams.query(initialParams.args);
                            const { params, ctx } = createParams(initialParams);
                            const { model } = params;
                            const operationChanged = params.operation !== initialParams.operation;
                            const result = operationChanged
                                ? // @ts-expect-error - we don't know what the client is
                                    await client[model[0].toLowerCase() + model.slice(1)][params.operation](params.args)
                                : await params.query(params.args);
                            const modifyResult = (_b = modifyResultByModel[params.model || ""]) === null || _b === void 0 ? void 0 : _b[params.operation];
                            if (!modifyResult)
                                return result;
                            return modifyResult(result, params, ctx);
                        },
                        async $allNestedOperations(initialParams) {
                            var _a, _b;
                            const createParams = (_a = createParamsByModel[initialParams.model || ""]) === null || _a === void 0 ? void 0 : _a[initialParams.operation];
                            if (!createParams)
                                return initialParams.query(initialParams.args);
                            const { params, ctx } = createParams(initialParams);
                            const result = await params.query(params.args, params.operation);
                            const modifyResult = (_b = modifyResultByModel[params.model || ""]) === null || _b === void 0 ? void 0 : _b[params.operation];
                            if (!modifyResult)
                                return result;
                            return modifyResult(result, params, ctx);
                        },
                    }),
                },
            },
        });
    });
}
