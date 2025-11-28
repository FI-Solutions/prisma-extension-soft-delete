import { ModelConfig } from "../types";
export declare function addDeletedToSelect<T extends {
    args?: any;
}>(params: T, config: ModelConfig): T;
export declare function stripDeletedFieldFromResults(results: any, config: ModelConfig): any;
export declare function isDeletedFieldOverWritten(field: string, where?: any): boolean;
